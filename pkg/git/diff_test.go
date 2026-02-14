package git

import (
	"testing"
)

const sampleDiff = `diff --git a/main.go b/main.go
new file mode 100644
index 0000000..abc1234
--- /dev/null
+++ b/main.go
@@ -0,0 +1,11 @@
+package main
+
+import "fmt"
+
+func main() {
+	fmt.Println("hello")
+}
+
+func helper() int {
+	return 42
+}
`

const modifiedDiff = `diff --git a/pkg/server.go b/pkg/server.go
index abc1234..def5678 100644
--- a/pkg/server.go
+++ b/pkg/server.go
@@ -10,7 +10,8 @@ func Start() {
 	mux := http.NewServeMux()
 	mux.HandleFunc("/", handleRoot)
-	mux.HandleFunc("/api", handleAPI)
+	mux.HandleFunc("/api/v1", handleAPIv1)
+	mux.HandleFunc("/api/v2", handleAPIv2)
 	log.Fatal(http.ListenAndServe(":8080", mux))
 }
`

const deletedDiff = `diff --git a/old.go b/old.go
deleted file mode 100644
index abc1234..0000000
--- a/old.go
+++ /dev/null
@@ -1,5 +0,0 @@
-package main
-
-func deprecated() {
-	// old code
-}
`

const renameDiff = `diff --git a/old_name.go b/new_name.go
similarity index 95%
rename from old_name.go
rename to new_name.go
index abc1234..def5678 100644
--- a/old_name.go
+++ b/new_name.go
@@ -1,4 +1,4 @@
 package main

-func oldFunc() {}
+func newFunc() {}
`

const multiFileDiff = `diff --git a/a.go b/a.go
new file mode 100644
index 0000000..1111111
--- /dev/null
+++ b/a.go
@@ -0,0 +1,3 @@
+package main
+
+func A() {}
diff --git a/b.go b/b.go
new file mode 100644
index 0000000..2222222
--- /dev/null
+++ b/b.go
@@ -0,0 +1,3 @@
+package main
+
+func B() {}
`

func TestParseNewFile(t *testing.T) {
	diff, err := ParseDiff(sampleDiff, "staged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if diff.Mode != "staged" {
		t.Errorf("expected mode staged, got %q", diff.Mode)
	}
	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}

	f := diff.Files[0]
	if f.Path != "main.go" {
		t.Errorf("expected path main.go, got %q", f.Path)
	}
	if f.Status != "added" {
		t.Errorf("expected status added, got %q", f.Status)
	}
	if len(f.Hunks) != 1 {
		t.Fatalf("expected 1 hunk, got %d", len(f.Hunks))
	}

	h := f.Hunks[0]
	if h.NewStart != 1 || h.NewCount != 11 {
		t.Errorf("expected new range 1,11 got %d,%d", h.NewStart, h.NewCount)
	}

	// All lines should be "added"
	addedCount := 0
	for _, line := range h.Lines {
		if line.Type == "added" {
			addedCount++
		}
	}
	if addedCount != 11 {
		t.Errorf("expected 11 added lines, got %d", addedCount)
	}
}

func TestParseModifiedFile(t *testing.T) {
	diff, err := ParseDiff(modifiedDiff, "unstaged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}

	f := diff.Files[0]
	if f.Path != "pkg/server.go" {
		t.Errorf("expected path pkg/server.go, got %q", f.Path)
	}
	if f.Status != "modified" {
		t.Errorf("expected status modified, got %q", f.Status)
	}

	h := f.Hunks[0]
	if h.OldStart != 10 || h.OldCount != 7 {
		t.Errorf("expected old range 10,7 got %d,%d", h.OldStart, h.OldCount)
	}
	if h.NewStart != 10 || h.NewCount != 8 {
		t.Errorf("expected new range 10,8 got %d,%d", h.NewStart, h.NewCount)
	}

	// Check we have added, removed, and context lines
	var added, removed, context int
	for _, line := range h.Lines {
		switch line.Type {
		case "added":
			added++
		case "removed":
			removed++
		case "context":
			context++
		}
	}
	if added != 2 {
		t.Errorf("expected 2 added lines, got %d", added)
	}
	if removed != 1 {
		t.Errorf("expected 1 removed line, got %d", removed)
	}
	if context < 3 {
		t.Errorf("expected at least 3 context lines, got %d", context)
	}
}

func TestParseDeletedFile(t *testing.T) {
	diff, err := ParseDiff(deletedDiff, "staged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}

	f := diff.Files[0]
	if f.Status != "deleted" {
		t.Errorf("expected status deleted, got %q", f.Status)
	}
}

func TestParseRenamedFile(t *testing.T) {
	diff, err := ParseDiff(renameDiff, "staged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}

	f := diff.Files[0]
	if f.Status != "renamed" {
		t.Errorf("expected status renamed, got %q", f.Status)
	}
	if f.OldPath != "old_name.go" {
		t.Errorf("expected old path old_name.go, got %q", f.OldPath)
	}
	if f.Path != "new_name.go" {
		t.Errorf("expected path new_name.go, got %q", f.Path)
	}
}

func TestParseMultipleFiles(t *testing.T) {
	diff, err := ParseDiff(multiFileDiff, "staged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(diff.Files) != 2 {
		t.Fatalf("expected 2 files, got %d", len(diff.Files))
	}

	if diff.Files[0].Path != "a.go" {
		t.Errorf("expected first file a.go, got %q", diff.Files[0].Path)
	}
	if diff.Files[1].Path != "b.go" {
		t.Errorf("expected second file b.go, got %q", diff.Files[1].Path)
	}
}

func TestParseEmptyDiff(t *testing.T) {
	diff, err := ParseDiff("", "staged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(diff.Files) != 0 {
		t.Errorf("expected 0 files, got %d", len(diff.Files))
	}
}

func TestLineNumbers(t *testing.T) {
	diff, err := ParseDiff(sampleDiff, "staged")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	h := diff.Files[0].Hunks[0]
	// New file starts at line 1
	for i, line := range h.Lines {
		if line.Type == "added" && line.NewNum != i+1 {
			t.Errorf("line %d: expected NewNum=%d, got %d", i, i+1, line.NewNum)
		}
	}
}
