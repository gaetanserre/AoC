set -x

cp lakefile_a.lean lakefile.lean
lake build
cp lakefile_b.lean lakefile.lean
lake build


./.lake/build/bin/seven_a
./.lake/build/bin/seven_b