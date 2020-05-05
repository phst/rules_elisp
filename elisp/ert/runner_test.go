// Copyright 2020 Google LLC
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
	"io/ioutil"
	"os"
	"os/exec"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/phst/runfiles"
)

func Test(t *testing.T) {
	bin, err := runfiles.Path("phst_rules_elisp/elisp/ert/test_test")
	if err != nil {
		t.Fatal(err)
	}
	runfilesEnv, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	reportFile, err := ioutil.TempFile(os.Getenv("TEST_TMPDIR"), "report-*.xml")
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

	cmd := exec.Command(bin)
	// See
	// https://docs.bazel.build/versions/3.1.0/test-encyclopedia.html#initial-conditions.
	cmd.Env = append(os.Environ(), append(runfilesEnv, "XML_OUTPUT_FILE="+reportName)...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
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

	b, err := ioutil.ReadFile(reportName)
	if err != nil {
		t.Fatal(err)
	}
	type testCase struct {
		Name      string  `xml:"name,attr"`
		ClassName string  `xml:"classname,attr"`
		Time      float64 `xml:"time,attr"`
	}
	type report struct {
		XMLName   xml.Name
		Name      string     `xml:"name,attr"`
		Tests     int        `xml:"tests,attr"`
		Errors    int        `xml:"errors,attr"`
		Failures  int        `xml:"failures,attr"`
		Skipped   int        `xml:"skipped,attr"`
		Time      float64    `xml:"time,attr"`
		Timestamp timestamp  `xml:"timestamp,attr"`
		TestCases []testCase `xml:"testcase"`
	}
	var got report
	if err := xml.Unmarshal(b, &got); err != nil {
		t.Error(err)
	}
	// Margin for time comparisons.  One hour is excessive, but we only
	// care about catching obvious bugs here.
	const margin = time.Hour
	// This, together with the EquateApprox below, ensures that the elapsed
	// time is nonnegative and below the margin.
	wantElapsed := margin.Seconds() / 2
	want := report{
		XMLName:   xml.Name{"", "testsuite"},
		Name:      "ERT",
		Tests:     9,
		Errors:    0,
		Failures:  6,
		Skipped:   1,
		Time:      wantElapsed,
		Timestamp: timestamp(time.Now()),
		TestCases: []testCase{
			{Name: "abort", ClassName: "ERT", Time: wantElapsed},
			{Name: "error", ClassName: "ERT", Time: wantElapsed},
			{Name: "expect-failure", ClassName: "ERT", Time: wantElapsed},
			{Name: "expect-failure-but-pass", ClassName: "ERT", Time: wantElapsed},
			{Name: "fail", ClassName: "ERT", Time: wantElapsed},
			{Name: "filter", ClassName: "ERT", Time: wantElapsed},
			{Name: "pass", ClassName: "ERT", Time: wantElapsed},
			{Name: "skip", ClassName: "ERT", Time: wantElapsed},
			{Name: "throw", ClassName: "ERT", Time: wantElapsed},
		},
	}
	if diff := cmp.Diff(got, want, cmp.Transformer("time.Time", toTime), cmpopts.EquateApprox(0, wantElapsed), cmpopts.EquateApproxTime(margin)); diff != "" {
		t.Errorf("-got +want:\n%s", diff)
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
