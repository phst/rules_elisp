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
	"context"
	"encoding/xml"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
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
		got := err.ExitCode()
		const want = 1
		if got != want {
			t.Errorf("test binary: got exit code %d, want %d", got, want)
		}
	default:
		t.Error(err)
	}
}

func TestReportValid(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "report.xml")
	runTest(t,
		// See
		// https://bazel.build/reference/test-encyclopedia#initial-conditions.
		"XML_OUTPUT_FILE="+file,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"TEST_TARGET=//tests:test_test",
	)

	schema, err := runfiles.Rlocation(*jUnitXsd)
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("validing XML report %s against schema %s", file, schema)
	cmd := exec.Command("xmllint", "--nonet", "--noout", "--schema", schema, file)
	if err := run(t, "xmllint", cmd); err != nil {
		t.Errorf("error validating XML report file: %s", err)
	}
	t.Log("XML validation complete")
}

func TestReportContent(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "report.xml")

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"XML_OUTPUT_FILE="+file,
		"TEST_TARGET=//tests:test_test",
	)

	got := parseReport(t, file)
	want := reportTemplate()
	if diff := cmp.Diff(got, want, reportOpts); diff != "" {
		t.Error("-got +want:\n", diff)
	}
}

func TestReportSharded(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "report.xml")
	statusFile := filepath.Join(dir, "shard-status")

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"XML_OUTPUT_FILE="+file,
		"TEST_TARGET=//tests:test_test",
		"TEST_SHARD_STATUS_FILE="+statusFile,
		"TEST_TOTAL_SHARDS=5",
		"TEST_SHARD_INDEX=2",
	)

	got := parseReport(t, file)
	want := reportTemplate()
	want.delete(
		"abort", "command-line", "error", "ert-fail",
		"expect-failure", "expect-failure-but-pass", "filter", "nocover",
		"pass", "skip", "throw",
	)
	want.Errors = 1
	want.Failures = 1
	want.Skipped = 0
	if diff := cmp.Diff(got, want, reportOpts); diff != "" {
		t.Error("-got +want:\n", diff)
	}

	stat, err := os.Lstat(statusFile)
	if err != nil {
		t.Fatalf("shard status file: %s", err)
	}
	if m := stat.Mode(); !m.IsRegular() {
		t.Errorf("shard status file %s isn’t regular; mode = %s", statusFile, m)
	}
}

func TestReportSkipTag(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "report.xml")

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"XML_OUTPUT_FILE="+file,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"TEST_TARGET=//tests:test_test",
	)

	got := parseReport(t, file)
	want := reportTemplate()
	want.delete("filter")
	want.Failures--
	if diff := cmp.Diff(got, want, reportOpts); diff != "" {
		t.Error("-got +want:\n", diff)
	}
}

func TestReportFailFast(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "report.xml")

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"XML_OUTPUT_FILE="+file,
		"TESTBRIDGE_TEST_RUNNER_FAIL_FAST=1",
		"TEST_TARGET=//tests:test_test",
	)

	got := parseReport(t, file)
	want := reportTemplate()
	want.delete(
		"ert-fail",
		"expect-failure",
		"expect-failure-but-pass",
		"fail",
		"filter",
		"nocover",
		"pass",
		"skip",
		"special-chars",
		"throw",
	)
	want.Errors = 1
	want.Failures = 0
	want.Skipped = 1
	if emacsVersion != "30.1" {
		want.delete(
			"command-line",
			"coverage",
			"error",
		)
		want.Skipped = 0
	}
	if diff := cmp.Diff(got, want, reportOpts); diff != "" {
		t.Error("-got +want:\n", diff)
	}
}

// Margin for time comparisons.  One hour is excessive, but we only
// care about catching obvious bugs here.
const margin = time.Hour

// This, together with the EquateApprox below, ensures that the elapsed
// time is nonnegative and below the margin.
var wantElapsed = margin.Seconds() / 2

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

func parseReport(t *testing.T, file string) report {
	t.Helper()
	t.Log("parsing XML report")
	b, err := os.ReadFile(file)
	if err != nil {
		t.Fatal(err)
	}
	var r report
	if err := xml.Unmarshal(b, &r); err != nil {
		t.Fatal(err)
	}
	return r
}

func reportTemplate() report {
	r := report{
		XMLName:    xml.Name{Local: "testsuite"},
		Name:       "ERT",
		Tests:      14,
		Errors:     4,
		Failures:   5,
		Skipped:    1,
		Time:       wantElapsed,
		Timestamp:  timestamp(time.Now()),
		Properties: properties{[]property{{"emacs-version", emacsVersion}}},
		TestCases: []testCase{
			{
				Name:      "abort",
				ClassName: "ERT",
				Time:      wantElapsed,
				Error: message{
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
				Error: message{
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
				Name:      "filter",
				ClassName: "ERT",
				Time:      wantElapsed,
				Failure: message{
					Message:     `Test failed: ((should (= 0 1)) :form (= 0 1) :value nil)`,
					Type:        `ert-test-failed`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "nocover",
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
				Error: message{
					Message:     "Error äöü \t \n \\u0000 \uFFFD \\uFFFE \\uFFFF 𝑨 <![CDATA[ ]]> & < > \" ' <!-- -->",
					Type:        `error`,
					Description: nonEmpty,
				},
			},
			{
				Name:      "throw",
				ClassName: "ERT",
				Time:      wantElapsed,
				Error: message{
					Message:     `No catch for tag: unknown-tag, hi`,
					Type:        `no-catch`,
					Description: nonEmpty,
				},
			},
		},
	}
	if emacsVersion == "30.1" {
		// https://bugs.gnu.org/76447
		abort := &r.TestCases[0]
		abort.Error = message{}
		abort.Skipped = message{
			Message:     `Test skipped: ((skip-unless (not (string-equal emacs-version "30.1"))) :form (not t) :value nil)`,
			Description: nonEmpty,
		}
		r.Errors--
		r.Skipped++
	}
	return r
}

func (r *report) delete(tests ...string) {
	oldLen := len(r.TestCases)
	r.TestCases = slices.DeleteFunc(r.TestCases, func(c testCase) bool {
		return slices.Contains(tests, c.Name)
	})
	r.Tests += len(r.TestCases) - oldLen
}

var reportOpts = cmp.Options{
	cmp.Transformer("time.Time", toTime),
	cmpopts.EquateApprox(0, wantElapsed),
	cmpopts.EquateApproxTime(margin),
}

func TestCoverage(t *testing.T) {
	tempDir := t.TempDir()
	manifest := filepath.Join(tempDir, "coverage-manifest.txt")
	t.Logf("writing coverage manifest %s", manifest)
	if err := os.WriteFile(manifest, []byte("tests/test-lib.el\nunrelated.el\n"), 0400); err != nil {
		t.Error(err)
	}
	coverageDir := t.TempDir()

	// See
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	runTest(t,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"TEST_TARGET=//tests:test_test",
		"COVERAGE=1",
		"COVERAGE_MANIFEST="+manifest,
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
	got := string(b)
	// See geninfo(1) for the coverage file format.  Depending on the exact
	// runfiles layout, the test runner might have printed different
	// representations of test filenames.
	got = regexp.MustCompile(`(?m)^(SF:).+[/\\](tests[/\\]test-lib\.el)$`).ReplaceAllString(got, "$1$2")
	if diff := cmp.Diff(got, wantCoverage); diff != "" {
		t.Error("-got +want:\n", diff)
	}

	if *regenerateCoverageDat {
		workspace := os.Getenv("BUILD_WORKSPACE_DIRECTORY")
		if workspace == "" {
			t.Fatal("to regenerate //tests:coverage.dat, run “bazel run -- //tests:go_default_test --regenerate-coverage-dat”")
		}
		if err := os.WriteFile(filepath.Join(workspace, "tests", "coverage.dat"), []byte(got), 0644); err != nil {
			t.Fatal(err)
		}
	}
}

func runTest(t *testing.T, testEnv ...string) error {
	t.Helper()
	d, ok := t.Deadline()
	if !ok {
		d = time.Now().Add(time.Minute)
	}
	ctx, cancel := context.WithDeadline(context.Background(), d.Add(-10*time.Second))
	defer cancel()
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

	cmd := exec.CommandContext(ctx, bin, "arg 1", "arg\n2 äα𝐴🐈'")
	// Only keep required variables from
	// https://bazel.build/reference/test-encyclopedia#initial-conditions.
	requiredEnv := []string{
		"LOGNAME",
		"TEST_SRCDIR",
		"TEST_TMPDIR",
		"TZ",
		"USER",
		"BAZEL_TEST",
	}
	env := slices.DeleteFunc(os.Environ(), func(s string) bool {
		name, _, _ := strings.Cut(s, "=")
		return !slices.Contains(requiredEnv, name)
	})
	env = append(env, rf.Env()...)
	env = append(env, testEnv...)
	cmd.Env = env
	cmd.Dir = workspace
	err = run(t, "Emacs", cmd)
	t.Log("test process exited, error:", err)
	return err
}

func run(t *testing.T, p string, c *exec.Cmd) error {
	t.Helper()
	if c.Stdout != nil {
		t.Fatalf("%s: exec.Cmd.Stdout already set", p)
	}
	if c.Stderr != nil {
		t.Fatalf("%s: exec.Cmd.Stderr already set", p)
	}
	r, err := c.StdoutPipe()
	if err != nil {
		t.Fatalf("%s: %s", p, err)
	}
	c.Stderr = c.Stdout
	if err := c.Start(); err != nil {
		t.Fatalf("%s: %s", p, err)
	}
	s := bufio.NewScanner(r)
	for s.Scan() {
		t.Logf("[%s] %s", p, s.Bytes())
	}
	if err := s.Err(); err != nil {
		t.Errorf("[%s] error: %s", p, err)
	}
	return c.Wait()
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

//go:embed version.txt
var emacsVersion string

//go:embed coverage.dat
var wantCoverage string
