# advent2025
Advent of Code 2025 in Scala

To create a new day
```bash
sbt new rmarbeck/advent-template.g8
```


To test all days, from root dir launch sbt test
```bash
sbt test
```

To select one day, in project root launch sbt and
```sbt
project adventdayXX
run | test
```
... or, change to dayXX and launch sbt
```bash
cd dayXX
sbt
```
