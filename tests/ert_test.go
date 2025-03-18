// Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package ert_test

import (
	"bufio"
	"encoding/xml"
	"flag"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"testing"
	"time"

	_ "embed"

	"github.com/bazelbuild/rules_go/go/runfiles"
	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

var (
	binary                = flag.String("binary", "", "location of the binary file relative to the runfiles root")
	testEl                = flag.String("test-el", "", "location of //tests:test.el relative to the runfiles root")
	jUnitXsd              = flag.String("junit-xsd", "", "location of @junit_xsd//:JUnit.xsd relative to the runfile root")
	regenerateCoverageDat = flag.Bool("regenerate-coverage-dat", false, "regenerate //tests:coverage.dat")
)

func TestExitCode(t *testing.T) {
	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	err := runTest(t,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"TEST_TARGET=//tests:test_test",
	)
	switch err := err.(type) {
	case nil:
		t.Error("test binary succeeded unexpectedly")
	case *exec.ExitError:
		if err.ExitCode() != 1 {
			t.Errorf("test binary: got exit code %d, want 1", err.ExitCode())
		}
	default:
		t.Error(err)
	}
}

func TestReportValid(t *testing.T) {
	tempDir := t.TempDir()
	reportName := filepath.Join(tempDir, "report.xml")
	runTest(t,
		// See
		// https://bazel.build/reference/test-encyclopedia#initial-conditions.
		"XML_OUTPUT_FILE="+reportName,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"TEST_TARGET=//tests:test_test",
	)

	schema, err := runfiles.Rlocation(*jUnitXsd)
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("validing XML report %s against schema %s", reportName, schema)
	r, w := io.Pipe()
	go log(t, "xmllint", r)
	cmd := exec.Command("xmllint", "--nonet", "--noout", "--schema", schema, reportName)
	cmd.Stdout = w
	cmd.Stderr = w
	err = cmd.Run()
	if err != nil {
		t.Errorf("error validating XML report file: %s", err)
	}
	t.Log("XML validation complete")
}

func TestReportContent(t *testing.T) {
	tempDir := t.TempDir()
	reportName := filepath.Join(tempDir, "report.xml")

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"XML_OUTPUT_FILE="+reportName,
		"TESTBRIDGE_TEST_ONLY=(not (or (tag skip) (tag :nocover)))",
		"TEST_TARGET=//tests:test_test",
	)

	t.Log("parsing XML report")
	b, err := os.ReadFile(reportName)
	if err != nil {
		t.Fatal(err)
	}
	type message struct {
		Message     string      `xml:"message,attr"`
		Type        string      `xml:"type,attr"`
		Description description `xml:",chardata"`
	}
	type property struct {
		Name  string `xml:"name,attr"`
		Value string `xml:"value,attr"`
	}
	type properties struct {
		Properties []property `xml:"property"`
	}
	type testCase struct {
		Name      string  `xml:"name,attr"`
		ClassName string  `xml:"classname,attr"`
		Time      float64 `xml:"time,attr"`
		Skipped   message `xml:"skipped"`
		Error     message `xml:"error"`
		Failure   message `xml:"failure"`
	}
	type report struct {
		XMLName    xml.Name
		Name       string     `xml:"name,attr"`
		Tests      int        `xml:"tests,attr"`
		Errors     int        `xml:"errors,attr"`
		Failures   int        `xml:"failures,attr"`
		Skipped    int        `xml:"skipped,attr"`
		Time       float64    `xml:"time,attr"`
		Timestamp  timestamp  `xml:"timestamp,attr"`
		Properties properties `xml:"properties"`
		TestCases  []testCase `xml:"testcase"`
	}
	var gotReport report
	err = xml.Unmarshal(b, &gotReport)
	if err != nil {
		t.Error(err)
	}
	var emacsVersion string
	for _, prop := range gotReport.Properties.Properties {
		if prop.Name == "emacs-version" {
			emacsVersion = prop.Value
			break
		}
	}
	t.Logf("got Emacs version %s", emacsVersion)
	// Margin for time comparisons.  One hour is excessive, but we only
	// care about catching obvious bugs here.
	const margin = time.Hour
	// This, together with the EquateApprox below, ensures that the elapsed
	// time is nonnegative and below the margin.
	wantElapsed := margin.Seconds() / 2
	wantReport := report{
		XMLName:    xml.Name{Local: "testsuite"},
		Name:       "ERT",
		Tests:      12,
		Errors:     0,
		Failures:   7,
		Skipped:    1,
		Time:       wantElapsed,
		Timestamp:  timestamp(time.Now()),
		Properties: properties{[]property{{"emacs-version", emacsVersion}}},
		TestCases: []testCase{
			{
				Name:      "abort",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     `peculiar error: "Boo"`,
					Type:        `undefined-error-symbol`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "command-line",
				ClassName: "ERT",
				Time:      wantElapsed,
			},
			{
				Name:      "coverage",
				ClassName: "ERT",
				Time:      wantElapsed,
			},
			{
				Name:      "error",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     `Boo`,
					Type:        `error`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "ert-fail",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     `Test failed: "Fail!"`,
					Type:        `ert-test-failed`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "expect-failure",
				ClassName: "ERT",
				Time:      wantElapsed,
			},
			{
				Name:      "expect-failure-but-pass",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message: `Test passed unexpectedly`,
					Type:    `error`,
				},
			},
			{
				Name:      "fail",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     `Test failed: ((should (= 0 1)) :form (= 0 1) :value nil)`,
					Type:        `ert-test-failed`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "pass",
				ClassName: "ERT",
				Time:      wantElapsed,
			},
			{
				Name:      "skip",
				ClassName: "ERT",
				Time:      wantElapsed,
				Skipped: message{
					Message:     "Test skipped: ((skip-unless (= 1 2)) :form (= 1 2) :value nil)",
					Description: nonEmpty,
				},
			},
			{
				Name:      "special-chars",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     "Error äöü \t \n \\u0000 \uFFFD \\uFFFE \\uFFFF 𝑨 <![CDATA[ ]]> & < > \" ' <!-- -->",
					Type:        `error`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "throw",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     `No catch for tag: unknown-tag, hi`,
					Type:        `no-catch`,
					Description: nonEmpty,
				},
			},
		},
	}
	if emacsVersion == "30.1" {
		// https://bugs.gnu.org/76447
		abort := &wantReport.TestCases[0]
		abort.Failure = message{}
		abort.Skipped = message{
			Message:     `Test skipped: ((skip-unless (not (string-equal emacs-version "30.1"))) :form (not t) :value nil)`,
			Description: nonEmpty,
		}
		wantReport.Failures--
		wantReport.Skipped++
	}
	if diff := cmp.Diff(
		gotReport, wantReport,
		cmp.Transformer("time.Time", toTime),
		cmpopts.EquateApprox(0, wantElapsed),
		cmpopts.EquateApproxTime(margin),
	); diff != "" {
		t.Error("XML test report (-got +want):\n", diff)
	}
}

func TestCoverage(t *testing.T) {
	tempDir := t.TempDir()
	coverageManifest := filepath.Join(tempDir, "coverage-manifest.txt")
	t.Logf("writing coverage manifest %s", coverageManifest)
	if err := os.WriteFile(coverageManifest, []byte("tests/test-lib.el\nunrelated.el\n"), 0400); err != nil {
		t.Error(err)
	}
	coverageDir := t.TempDir()

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"TEST_TARGET=//tests:test_test",
		"COVERAGE=1",
		"COVERAGE_MANIFEST="+coverageManifest,
		"COVERAGE_DIR="+coverageDir,
	)

	t.Logf("looking for coverage files in %s", coverageDir)
	files, err := filepath.Glob(filepath.Join(coverageDir, "e*.dat"))
	if err != nil {
		t.Error(err)
	}
	if len(files) != 1 {
		t.Fatalf("got %d coverage files %v, want exactly one", len(files), files)
	}
	b, err := os.ReadFile(files[0])
	if err != nil {
		t.Error(err)
	}
	gotCoverage := string(b)
	// See geninfo(1) for the coverage file format.  Depending on the exact
	// runfiles layout, the test runner might have printed different
	// representations of test filenames.
	gotCoverage = regexp.MustCompile(`(?m)^(SF:).+[/\\](tests[/\\]test-lib\.el)$`).ReplaceAllString(gotCoverage, "$1$2")
	if diff := cmp.Diff(gotCoverage, wantCoverage); diff != "" {
		t.Error("coverage report (-got +want):\n", diff)
	}

	if *regenerateCoverageDat {
		workspace := os.Getenv("BUILD_WORKSPACE_DIRECTORY")
		if workspace == "" {
			t.Fatal("to regenerate //tests:coverage.dat, run “bazel run -- //tests:go_default_test --regenerate-coverage-dat”")
		}
		if err := os.WriteFile(filepath.Join(workspace, "tests", "coverage.dat"), []byte(gotCoverage), 0644); err != nil {
			t.Fatal(err)
		}
	}
}

func runTest(t *testing.T, testEnv ...string) error {
	t.Helper()
	rf, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	source, err := rf.Rlocation(*testEl)
	if err != nil {
		t.Fatal(err)
	}
	// Use the ancestor of the source file as repository directory so that
	// filenames in the coverage report are correct.
	workspace := filepath.Dir(filepath.Dir(source))
	t.Logf("running test in workspace directory %s", workspace)
	bin, err := rf.Rlocation(*binary)
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("using test binary %s", bin)

	r, w := io.Pipe()
	go log(t, "Emacs", r)

	cmd := exec.Command(bin, "arg 1", "arg\n2 äα𝐴🐈'")
	env := os.Environ()
	env = append(env, rf.Env()...)
	env = append(env, testEnv...)
	cmd.Env = env
	cmd.Stdout = w
	cmd.Stderr = w
	cmd.Dir = workspace
	err = cmd.Run()
	t.Log("test process exited, error:", err)
	return err
}

func log(t *testing.T, p string, r io.Reader) {
	s := bufio.NewScanner(r)
	for s.Scan() {
		t.Logf("[%s] %s", p, s.Bytes())
	}
	if err := s.Err(); err != nil {
		t.Errorf("[%s] error: %s", p, err)
	}
}

type timestamp time.Time

func (t *timestamp) UnmarshalText(b []byte) error {
	// The XML report format doesn’t allow timezones in timestamps, so
	// time.Time.UnmarshalText doesn’t work.
	u, err := time.Parse("2006-01-02T15:04:05", string(b))
	*t = timestamp(u)
	return err
}

func toTime(t timestamp) time.Time { return time.Time(t) }

type description bool

const (
	empty    description = false
	nonEmpty description = true
)

func (d *description) UnmarshalText(b []byte) error {
	// We only check that the description isn’t absent or empty.
	*d = len(b) > 0
	return nil
}

func (d description) String() string {
	if d == nonEmpty {
		return "something"
	}
	return ""
}

//go:embed coverage.dat
var wantCoverage string
