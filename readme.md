# Function `determine.allen.relation()`

Compare two indefinite intervals produced by a Bayesian calibration using the Allen algebra, display a graphical summary of the Allen relation set based on the N&ouml;kel lattice, optionally save the graphical summary to a file, and return a list containing the frequency of each basic relation in the Bayesian calibration, the Allen relation set, a statement whether or not the Allen relation set is concurrent, the basic relation frequencies expressed as proportions, the proportion of concurrent relations in the Bayesian calibration, and the possible inferences that can be made with the Allen algebra about relations between one of the intervals and a context related stratigraphically to the other interval.

# Description

Parameters include one required file path, `mcmc.file`, and one optional file path, `mcmc.file.2`. The chains in `mcmc.file` are read from the columns indexed in the required parameter, `positions`. The chains in `mcmc.file.2` are read from the columns indexed in the optional parameter, `positions.2`. The optional parameter, `iteration.column`, is passed to the `iterationColumn` parameter of the `ArchaeoPhases ImportCSV()` function, q.v. It defaults to 1. If the function is called with two files, `positions` and `positions.2` must be vectors of length 2. If the function is called with one file, then `positions` must be a vector of length 4. The function checks for missing or incorrect parameters and whether or not files exist. It exits with an error message if there are problems.

A side-effect of the function is a graphical summary of the relation plotted on the display. Colors are chosen from one of four eight-color palettes, specified by the `palette` argument. The color of a node on the graph is chosen based on the cubed root of the frequency with which it was realized in the Bayesian calibration. High frequencies are colored darker than low frequencies. A graph title can be specified with the `main` argument, and a subtitle with the `sub` argument.

If the `out.file` argument is set to a valid file name (with suffix), then the graphical summary will be written to this file in the graphics format specified by `file.type`. If `file.type` is not recognized, then the function exits with an error message.

A six valued algebra can be used by setting the `six.value` flag. The flag is `FALSE` by default, which selects the standard 13 valued algebra.

It is sometimes the case that the indeterminate relations between two intervals estimated by Bayesian calibration either fail to support inferences with the Allen algebra or yield relatively weak relation sets. In these instances, it is sometimes possible to make or strengthen inferences by limiting the relation set to the most popular basic relations. The `threshold` argument takes a value &gt; 0 and &lt; 1 to indicate the minimum proportion of observations the relation set must comprise.

# Arguments

-   **`mcmc.file`:** path to a csv file produced by Bayesian calibration software;
-   **`positions`:** a vector of column numbers in `mcmc.file`;
-   **`mcmc.file.2`:** an optional path to a csv file produced by Bayesian calibration software;
-   **`positions.2`:** an optional vector of column numbers in `mcmc.file.2`;
-   **`out.file`:** optional path to an output file, which is produced only if the argument is set;
-   **`file.type`:** graphics file type suffix, one of 'pdf' (default), 'png', 'jpg', 'jpeg', 'postscript', 'ps', 'eps', 'svg', 'tiff', 'tif', 'bmp', 'pictex', 'xfig', 'bitmap', or 'bmp';
-   **`palette`:** specify the color palette to fill graph nodes, one of 'grey' (default), 'blue', 'red', or 'green';
-   **`main`:** a string to use as the graph title;
-   **`sub`:** a string to use as the graph subtitle;
-   **`six.value`:** a flag to toggle use of a six valued algebra; and
-   **`threshold`:** the proportion of relations to include in an uncertain set, default = 0.95.

# Return Value

A list with the following components:

-   **`allen.result`:** frequencies of basic relations between two intervals in the Bayesian calibration;
-   **`allen.set`:** the basic relations represented in the Bayesian calibration;
-   **`allen.uncertain.set`:** the basic relations represented in a portion of the Bayesian calibration;
-   **`allen.concurrence`:** a statement whether or not `allen.set` comprises concurrent relations;
-   **`allen.proportion`:** proportions of frequencies for relations in `allen.set`;
-   **`allen.proportion.concurs`:** proportion of the concurrent relations between the two intervals in the Bayesian calibration;
-   **`allen.1.to.precedes.2`:** inference about the relation between one interval and a context that precedes the other interval, i.e., deposits stratigraphically inferior to the context represented by the interval;
-   **`allen.1.to.meets.2`:** inference about the relation between one interval and a context that meets the other interval; i.e., the interface between the context represented by the interval and underlying deposits;
-   **`allen.1.to.preceded.by.2`:** inference about the relation between one interval and a context that is preceded by the other interval; i.e., deposits stratigraphically superior to the context represented by the interval;
-   **`allen.1.to.met.by.2`:** inference about the relation between one interval and a context that is met by the other interval; i.e, the interface between the context represented by the interval and overlying deposits;
-   **`allen.1.to.precedes.2.uncertain`:** uncertain inference about the relation between one interval and a context that precedes the other interval, i.e., deposits stratigraphically inferior to the context represented by the interval;
-   **`allen.1.to.meets.2.uncertain`:** uncertain inference about the relation between one interval and a context that meets the other interval; i.e., the interface between the context represented by the interval and underlying deposits;
-   **`allen.1.to.preceded.by.2.uncertain`:** uncertain inference about the relation between one interval and a context that is preceded by the other interval; i.e., deposits stratigraphically superior to the context represented by the interval; and
-   **`allen.1.to.met.by.2.uncertain`:** uncertain inference about the relation between one interval and a context that is met by the other interval; i.e, the interface between the context represented by the interval and overlying deposits.

# Dependencies

The function depends on the `igraph`, `ArchaeoPhases`, and `Xmisc` packages, and will attempt to install them if not found. The function checks if the `igraph` and `ArchaeoPhases` packages are loaded before attempting to load them.
