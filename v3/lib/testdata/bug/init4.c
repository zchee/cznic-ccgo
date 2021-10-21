#include <stdint.h>

typedef union {
	struct {
		char type;
		short state;
		char extra;
		char repetition;
	} shift;
	struct {
		char type;
		char child_count;
		short symbol;
		short dynamic_precedence;
		short production_id;
	} reduce;
	char type;
} TSParseAction;

typedef union {
	TSParseAction action;
	struct {
		char count;
		char reusable;
	} entry;
} TSParseActionEntry;

static const TSParseActionEntry ts_parse_actions[] = {
	[14] =
		{
			{
				.reduce = {
					.type = 33,
				},
			}
		},
};

// A sub-issue of init3.c: Before we fix it, the produced code is wrong on many
// levels:
//
//	type TSParseAction = struct {
//		shift struct {
//			__type     int8
//			_          [1]byte
//			state      int16
//			extra      int8
//			repetition int8
//		}
//		_ [2]byte
//	} /* init4.c:18:3 */
//
// Error: Size of TSParseAction is 8, should be 10.
//	
// Error: We should not define unions using any of its fields, but make it a
// alignment-enforcing pseudo field followed by a [n]byte array. The
// readability was nice, but we have to revert to document the C layout in
// comments only.
//
//	var ts_parse_actions = [1]TSParseActionEntry{{
//		action: TSParseAction{shift: struct {
//			__type     int8
//			_          [1]byte
//			state      int16
//			extra      int8
//			repetition int8
//		}{__type: int8(33)},
//		},
//	},
//	} /* init4.c:28:33 */
//
// Error: TSParseActionEntry is an union. We cannot initialize it using a Go
// literal in the general case.
//
// Error: We initialize the 'shift' field, but the initializer initializes the
// 'reduce' field. Probably rooted in code written before adding support for
// GCC initializer extensions. C99 standard allows to initialize only the first
// field of an union.
//
// Error: The ts_parse_actions array should have 15 elements, but it has only 1.

int main() {
	if (ts_parse_actions[14].action.reduce.type != 33) {
		return __LINE__;
	}

	return 0;
}
