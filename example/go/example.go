// Copyright 2015 Google Inc.
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// USAGE
//
//   Create coverage overlays
//    - Install Go (https://golang.org/dl/)
//    - Run cov-show command (M-x cov-show RET)
//
//   Refresh coverage overlays
//    - Modify the buffer while overlays are shown
//    - Rerun the Go coverage by recompiling (M-x recompile RET)
//       - This should rerun 'go test -covermode=count'
//    - Overlays are automatically updated
//
//   Remove coverage overlays
//    - Run cov-hide to remove all overlays

package example

func foo(n int) int {
	var c int
	switch {
	case n < 10:
		c++
	case n < 25:
		c++
	case n < 50:
		c++
	case n < 100:
		c++
	case n < 1000:
		c++
	}
	return c
}
