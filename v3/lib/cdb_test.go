package ccgo

import (
	"bytes"
	"encoding/json"
	"reflect"
	"strings"
	"testing"
)

func TestCDBWriter(t *testing.T) {
	var b bytes.Buffer

	wr := newCDBWriter(&b)

	items := []cdbItem{
		{
			Arguments: []string{"hello", "there"},
			Directory: "/work",
		},
		{
			Arguments: []string{"good", "bye"},
			Directory: "/work/src",
		},
	}

	for _, it := range items {
		wr.add(it)
	}

	if err := wr.finish(); err != nil {
		t.Fatal(err)
	}

	var got []cdbItem
	if err := json.Unmarshal(b.Bytes(), &got); err != nil {
		t.Fatal(err)
	}

	if !reflect.DeepEqual(got, items) {
		t.Errorf("got items\n%#v\nwant\n%#v", got, items)
	}

	if !strings.HasPrefix(b.String(), "[\n  ") {
		t.Errorf("got non-pretty-printed output:\n%s", b.String())
	}
}
