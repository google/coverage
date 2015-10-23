// -*- compile-command: "gcc -coverage example.c" -*-

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
//    - Compile with "-coverage" flag (gcc -coverage example.c)
//    - Run the resulting binary (./a.out)
//    - Run cov-show command (M-x cov-show RET)
//
//   Remove coverage overlays
//    - Run cov-hide to remove all overlays

void foo(int i) {
  if (i > 1000) {
    return;
  }
  if (i < 50) {
    return;
  }
  if (i < 75) {
    return;
  }
  if (i < 90) {
    return;
  }
  if (i < 100) {
    return;
  }
  return;
}

int main(void) {
  for (int i = 0; i < 100; i++) {
    foo(i);
  }
  return 0;
}
