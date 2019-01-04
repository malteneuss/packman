# Packman 
A simple terminal 0/1 knapsack problem solver written in Haskell for the  "Mai 2017 IT-talents.de" code competition

## Installation
Packman is written in Haskell and uses the widespread Haskell "stack" build tool, so
all you need to do is install stack on your system by following the instructions on the
[Stack website](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Open the Packman project folder (where you find a `stack.yaml` file) in
a terminal and run
```bash
stack setup && stack build
```
This will download the Haskell GHC compiler and all dependent packages/libraries
into a hidden folder inside `.stack-work/install` and compile the Packman executable into
a hidden folder inside `.stack-work/dist/` folder.


## Usage
Inside the Packman folder you can run the Packman executable with
```bash
stack exec -- packman --help
```
or use the provided executable (which has been compiled for Linux only) with
```bash
./packman --help
```
`stack exec` will automatically locate the compiled packman executable inside the hidden build folder
so you don't have to.
`--help` will give you a detailed explanation of the features you can choose.

The general usage form is:
```
stack exec -- packman <input method> <solver> <outputfile>
```

### Input method
There are three ways to load a knapsack problem instance.
* From a file with `--file <fileWithInstance>`
* From standard input with `--stdin`
* Generate a random instance with `--random <number of items>`

The file format is as follows:
```
<itemCount> <capacity>
<values>
<weights>
```
For example:
```
3 5
1 2 3
3 2 1
```
This instance has 3 items and capacity 5
Item 0 has value 1, item 1 has 2 and item 2 has 3
Item 0 has weight 3, item 1 has 2 and item 2 has 1
There has to be a value and a weight for every item!"

You can choose to save the loaded or generated knapsack instance
to an output file with `--save <file>`.
This is handy if you want to solve it again later, e.g. with another solver.

### Solver
You MUST specify a solver to work on a knapsack instance.
There are three solver available with different performances and accuracies.
* A greedy solver that has the fastest performance and general good solutions, but without
any guarantees of optimality. Use it with `--greedy`
* An optimal solver that is guaranteed to give the best possible solution, but also
has the worst performance. Use it with `--optimal`
* An FPTAS (Fully Polynomial Approximation Scheme) solver that takes in
a deviation factor `epsilon` to fine-tune the trade-off between accuracy and performance.
Use it with `--fptas <epsilon>`. Epsilon must be a floating point value in the range (0,1].
It defines how many percent a solution is allowed to deviate from the optimum, e.g.
`epsilon == 0.1` means that the provided solution will at most deviate 10% from the optimum,
so a smaller `epsilon` provides a better solution but at the cost of worse performance.

Packman will present a solution succintly on the terminal.
To get a more detailed presentation of the solution you can use `--verbose`.

### Examples

```
stack exec -- packman --random 100 --greedy --save random.knap
```
This will generate a random instance with 100 items, solve it using the greedy solver
and save the generated instance to a random.knap file

```
stack exec -- packman --stdin --optimal --verbose
```
This will wait for you to type in an instance in the specified file format, solve it
using the optimal solver and print a more detailed solution.

```
stack exec -- packman --file random.knap --fptas 0.05
```
This will load the specified instance in the specified file format from a file, solve it
using the FPTAS solver and provide a solution with a worst-case deviation of 5% from
the optimum.

## Source code
The main solver algorithms are implemented in `src/Knapsack.hs`
and are variants from [Wikipedia](https://en.wikipedia.org/wiki/Knapsack_problem).
The terminal user interface is implemented in `app/Main.hs`.
Both files should be self-explanatory.

