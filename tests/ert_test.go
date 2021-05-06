// Copyright 2020, 2021 Google LLC
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

package runner_test

import (
	"encoding/xml"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/phst/runfiles"
)

func Test(t *testing.T) {
	workspace, err := runfiles.Path("phst_rules_elisp")
	if err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(workspace, "tests/test_test")
	runfilesEnv, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	tempDir := os.Getenv("TEST_TMPDIR")
	reportFile, err := ioutil.TempFile(tempDir, "report-*.xml")
	if err != nil {
		t.Fatal(err)
	}
	reportName := reportFile.Name()
	// The test recreates the file in exclusive mode, so delete it now.
	if err := os.Remove(reportName); err != nil {
		t.Error(err)
	}
	if err := reportFile.Close(); err != nil {
		t.Error(err)
	}
	coverageManifest, err := ioutil.TempFile(tempDir, "coverage-manifest-*.txt")
	if err != nil {
		t.Fatal(err)
	}
	defer coverageManifest.Close()
	defer os.Remove(coverageManifest.Name())
	if _, err := io.WriteString(coverageManifest, "tests/test-lib.el\nunrelated.el\n"); err != nil {
		t.Error(err)
	}
	coverageDir, err := ioutil.TempDir(tempDir, "coverage-")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(coverageDir)

	cmd := exec.Command(bin, "arg 1", "arg\n2")
	// See
	// https://docs.bazel.build/versions/3.1.0/test-encyclopedia.html#initial-conditions.
	cmd.Env = append(os.Environ(), append(runfilesEnv,
		"XML_OUTPUT_FILE="+reportName,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"COVERAGE=1",
		"COVERAGE_MANIFEST="+coverageManifest.Name(),
		"COVERAGE_DIR="+coverageDir)...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Dir = workspace
	switch err := cmd.Run().(type) {
	case nil:
		t.Error("test binary succeeded unexpectedly")
	case *exec.ExitError:
		if err.ExitCode() != 1 {
			t.Errorf("test binary: got exit code %d, want 1", err.ExitCode())
		}
	default:
		t.Error(err)
	}

	schema, err := runfiles.Path("junit_xsd/file/JUnit.xsd")
	if err != nil {
		t.Fatal(err)
	}
	cmd = exec.Command("xmllint", "--nonet", "--noout", "--schema", schema, reportName)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Errorf("error validating XML report file: %s", err)
	}

	b, err := ioutil.ReadFile(reportName)
	if err != nil {
		t.Fatal(err)
	}
	type message struct {
		Message     string `xml:"message,attr"`
		Type        string `xml:"type,attr"`
		Description string `xml:",chardata"`
	}
	isDescription := func(p cmp.Path) bool {
		f, ok := p.Last().(cmp.StructField)
		return ok && f.Name() == "Description"
	}
	type property struct {
		Name  string `xml:"name,attr"`
		Value string `xml:"value,attr"`
	}
	type properties struct {
		Properties []property `xml:"property"`
	}
	type testCase struct {
		Name      string    `xml:"name,attr"`
		ClassName string    `xml:"classname,attr"`
		Time      float64   `xml:"time,attr"`
		Skipped   *struct{} `xml:"skipped"`
		Error     message   `xml:"error"`
		Failure   message   `xml:"failure"`
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
	if err := xml.Unmarshal(b, &gotReport); err != nil {
		t.Error(err)
	}
	var emacsVersion string
	for _, prop := range gotReport.Properties.Properties {
		if prop.Name == "emacs-version" {
			emacsVersion = prop.Value
			break
		}
	}
	if !regexp.MustCompile(`^\d+\.\d+`).MatchString(emacsVersion) {
		t.Errorf("invalid Emacs version %q", emacsVersion)
	}
	// Margin for time comparisons.  One hour is excessive, but we only
	// care about catching obvious bugs here.
	const margin = time.Hour
	// This, together with the EquateApprox below, ensures that the elapsed
	// time is nonnegative and below the margin.
	wantElapsed := margin.Seconds() / 2
	wantReport := report{
		XMLName:    xml.Name{"", "testsuite"},
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
				Name: "abort", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: `peculiar error: "Boo"`, Type: `undefined-error-symbol`, Description: "something"},
			},
			{Name: "command-line", ClassName: "ERT", Time: wantElapsed},
			{Name: "coverage", ClassName: "ERT", Time: wantElapsed},
			{
				Name: "error", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: `Boo`, Type: `error`, Description: "something"},
			},
			{
				Name: "ert-fail", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: `Test failed: "Fail!"`, Type: `ert-test-failed`, Description: "something"},
			},
			{Name: "expect-failure", ClassName: "ERT", Time: wantElapsed},
			{
				Name: "expect-failure-but-pass", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: `Test passed unexpectedly`, Type: `error`},
			},
			{
				Name: "fail", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: `Test failed: ((should (= 0 1)) :form (= 0 1) :value nil)`, Type: `ert-test-failed`, Description: "something"},
			},
			{Name: "pass", ClassName: "ERT", Time: wantElapsed},
			{Name: "skip", ClassName: "ERT", Time: wantElapsed, Skipped: new(struct{})},
			{
				Name: "special-chars", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: "Error äöü \t \n \\u0000 \uFFFD \\uFFFE \\uFFFF 𝑨 <![CDATA[ ]]> & < > \" ' <!-- -->", Type: `error`, Description: "something"},
			},
			{
				Name: "throw", ClassName: "ERT", Time: wantElapsed,
				Failure: message{Message: `No catch for tag: unknown-tag, hi`, Type: `no-catch`, Description: "something"},
			},
		},
	}
	if diff := cmp.Diff(
		gotReport, wantReport,
		cmp.Transformer("time.Time", toTime), cmpopts.EquateApprox(0, wantElapsed), cmpopts.EquateApproxTime(margin),
		// We only check that the description isn’t absent or empty.
		cmp.FilterPath(isDescription, cmp.Comparer(bothEmpty)),
	); diff != "" {
		t.Error("XML test report (-got +want):\n", diff)
	}

	files, err := filepath.Glob(filepath.Join(coverageDir, "*.dat"))
	if err != nil {
		t.Error(err)
	}
	if len(files) != 1 {
		t.Fatalf("got %d coverage files %v, want exactly one", len(files), files)
	}
	b, err = ioutil.ReadFile(files[0])
	if err != nil {
		t.Error(err)
	}
	gotCoverage := string(b)
	// See geninfo(1) for the coverage file format.  The function hit count
	// doesn’t work yet for nested functions.  The jump in the suffix index
	// for the nested functions is due to
	// https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41988.
	wantCoverage := `SF:tests/test-lib.el
FN:26,tests/test-function
FN:38,foo@cl-flet@1
FN:40,foo@cl-flet@3
FNDA:1,tests/test-function
FNDA:0,foo@cl-flet@1
FNDA:0,foo@cl-flet@3
FNF:3
FNH:1
BRDA:29,0,0,0
BRDA:29,0,1,1
BRDA:33,0,0,1
BRDA:33,0,1,0
BRDA:43,0,0,0
BRDA:43,0,1,1
BRDA:43,0,2,0
BRDA:45,0,0,0
BRDA:45,0,1,1
BRDA:45,0,2,1
BRDA:45,0,3,0
BRDA:45,1,0,1
BRDA:45,1,1,0
BRDA:47,0,0,1
BRDA:47,0,1,0
BRF:15
BRH:7
DA:29,1
DA:30,1
DA:31,0
DA:32,1
DA:33,1
DA:34,1
DA:35,1
DA:36,0
DA:38,1
DA:39,1
DA:40,1
DA:41,1
DA:43,1
DA:44,1
DA:45,1
DA:46,0
DA:47,1
DA:48,1
DA:49,0
LH:15
LF:19
end_of_record
`
	if strings.HasPrefix(emacsVersion, "26.") {
		// No branch coverage under Emacs 26 expected.
		wantCoverage = regexp.MustCompile(`(?m)^BR.+\n`).ReplaceAllLiteralString(wantCoverage, "")
	}
	if diff := cmp.Diff(gotCoverage, wantCoverage); diff != "" {
		t.Error("coverage report (-got +want):\n", diff)
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

func bothEmpty(a, b string) bool { return (a == "") == (b == "") }
