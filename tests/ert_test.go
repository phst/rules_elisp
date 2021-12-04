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
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/phst/runfiles"
)

func Test(t *testing.T) {
	source, err := runfiles.Path("phst_rules_elisp/tests/test.el")
	if err != nil {
		t.Fatal(err)
	}
	// Use the ancestor of the source file as workspace directory so that
	// filenames in the coverage report are correct.
	workspace := filepath.Dir(filepath.Dir(source))
	t.Logf("running test in workspace directory %s", workspace)
	bin := "phst_rules_elisp/tests/test_test"
	if runtime.GOOS == "windows" {
		bin += ".exe"
	}
	bin, err = runfiles.Path(bin)
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("using test binary %s", bin)
	runfilesEnv, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	tempDir := t.TempDir()
	reportName := filepath.Join(tempDir, "report.xml")
	coverageManifest := filepath.Join(tempDir, "coverage-manifest.txt")
	t.Logf("writing coverage manifest %s", coverageManifest)
	if err := os.WriteFile(coverageManifest, []byte("tests/test-lib.el\nunrelated.el\n"), 0400); err != nil {
		t.Error(err)
	}
	coverageDir := t.TempDir()

	cmd := exec.Command(bin, "arg 1", "arg\n2")
	env := os.Environ()
	env = append(env, runfilesEnv...)
	// See
	// https://docs.bazel.build/versions/4.1.0/test-encyclopedia.html#initial-conditions.
	env = append(env,
		"XML_OUTPUT_FILE="+reportName,
		"TESTBRIDGE_TEST_ONLY=(not (tag skip))",
		"COVERAGE=1",
		"COVERAGE_MANIFEST="+coverageManifest,
		"COVERAGE_DIR="+coverageDir,
		// Don’t have the Python launcher change the current directory,
		// otherwise our fake environment setup won’t work.  See
		// https://github.com/bazelbuild/bazel/issues/7190.
		"RUN_UNDER_RUNFILES=0",
	)
	cmd.Env = env
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
	t.Log("test process exited")

	schema, err := runfiles.Path("junit_xsd/file/JUnit.xsd")
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("validing XML report %s against schema %s", reportName, schema)
	cmd = exec.Command("xmllint", "--nonet", "--noout", "--schema", schema, reportName)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Errorf("error validating XML report file: %s", err)
	}
	t.Log("XML validation complete")

	t.Log("parsing XML report")
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
	match := regexp.MustCompile(`^(\d+)\.\d+`).FindStringSubmatch(emacsVersion)
	if match == nil {
		t.Fatalf("invalid Emacs version %q", emacsVersion)
	}
	emacsMajor, err := strconv.Atoi(match[1])
	if err != nil {
		t.Errorf("invalid Emacs version %q", emacsVersion)
	}
	t.Logf("got Emacs version %s", emacsVersion)
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

	t.Logf("looking for coverage files in %s", coverageDir)
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
	// doesn’t work yet for nested functions.
	wantCoverage := `SF:tests/test-lib.el
FN:27,tests/test-function
FN:39,foo@cl-flet@0
FN:41,foo@cl-flet@1
FNDA:1,tests/test-function
FNDA:0,foo@cl-flet@0
FNDA:0,foo@cl-flet@1
FNF:3
FNH:1
BRDA:30,0,0,0
BRDA:30,0,1,1
BRDA:34,0,0,1
BRDA:34,0,1,0
BRDA:44,0,0,0
BRDA:44,0,1,1
BRDA:44,0,2,0
BRDA:44,0,3,0
BRDA:46,0,0,0
BRDA:46,0,1,1
BRDA:46,0,2,1
BRDA:46,0,3,0
BRDA:46,1,0,1
BRDA:46,1,1,0
BRDA:48,0,0,-
BRDA:48,0,1,-
BRDA:49,0,0,1
BRDA:49,0,1,0
BRDA:52,0,0,0
BRDA:52,0,1,1
BRDA:53,0,0,0
BRDA:53,0,1,1
BRDA:53,0,2,0
BRDA:53,0,3,0
BRDA:54,0,0,3
BRDA:54,0,1,0
BRF:26
BRH:10
DA:30,1
DA:31,1
DA:32,0
DA:33,1
DA:34,1
DA:35,1
DA:36,1
DA:37,0
DA:39,1
DA:40,1
DA:41,1
DA:42,1
DA:44,1
DA:45,1
DA:46,1
DA:47,0
DA:48,0
DA:49,1
DA:50,1
DA:51,0
DA:52,1
DA:53,1
DA:54,3
DA:56,1
LH:19
LF:24
end_of_record
`
	if emacsMajor == 26 {
		// No branch coverage under Emacs 26 expected.
		t.Log("not checking for branch coverage")
		wantCoverage = regexp.MustCompile(`(?m)^BR.+\n`).ReplaceAllLiteralString(wantCoverage, "")
	}
	if emacsMajor < 28 {
		// Account for a jump in the suffix index for nested functions
		// due to https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41988.
		wantCoverage = replaceSubmatch(wantCoverage, `@cl-flet@(\d+)`, func(s string) string {
			i, err := strconv.Atoi(s)
			if err != nil {
				t.Errorf("invalid suffix in coverage manifest: %s", err)
			}
			return strconv.Itoa(2*i + 1)
		})
		// In Emacs 27, the special when-let form on line 52 of
		// test-lib.el isn’t properly instrumented, so remove the
		// branches for that line, and decrement the branch counts
		// accordingly.
		wantCoverage = regexp.MustCompile(`(?m)^BRDA:52,.+\n`).ReplaceAllLiteralString(wantCoverage, "")
		wantCoverage = replaceSubmatch(wantCoverage, `(?m)^BRF:(\d+)$`, func(s string) string {
			i, err := strconv.Atoi(s)
			if err != nil {
				t.Errorf("invalid branch count in coverage manifest: %s", err)
			}
			return strconv.Itoa(i - 2)
		})
		wantCoverage = replaceSubmatch(wantCoverage, `(?m)^BRH:(\d+)$`, func(s string) string {
			i, err := strconv.Atoi(s)
			if err != nil {
				t.Errorf("invalid branch hit count in coverage manifest: %s", err)
			}
			return strconv.Itoa(i - 1)
		})
	}
	// Depending on the exact runfiles layout, the test runner might have
	// printed different representations of test filenames.
	gotCoverage = regexp.MustCompile(`(?m)^(SF:).+[/\\](tests[/\\]test-lib\.el)$`).ReplaceAllString(gotCoverage, "$1$2")
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

// replaceSubmatch transforms the strings matched by the first subgroup in the
// given regular expression through the given function.
func replaceSubmatch(text string, rgx string, fun func(string) string) string {
	r := regexp.MustCompile(rgx)
	if r.NumSubexp() != 1 {
		panic(fmt.Errorf("want exactly one subexpression in regular expression %q, got %d", r, r.NumSubexp()))
	}
	return r.ReplaceAllStringFunc(text, func(match string) string {
		indices := r.FindStringSubmatchIndex(match)
		if len(indices) != 4 || indices[0] != 0 || indices[1] != len(match) {
			panic(fmt.Errorf("invalid regular expression %q for match %q", r, match))
		}
		i, j := indices[2], indices[3]
		return match[:i] + fun(match[i:j]) + match[j:]
	})
}
