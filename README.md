# Streamsep

Library for polyphonic stream segregation in OpenMusic.

Includes a subclass of 'analysis' to work interactively with stream separation within segments.

Check the tutorial files for examples of usage.

Install in the usual ways for OM libraries.

Anders Vinjar, 2017

---------

The terms 'Stream separation' or 'Stream segregation" are used here,
meaning more or less the same as polyphonic voice separation or voice
following.

The core algorithm is based on Machine Learning clustering techniques,
using 'Single Link Agglomerative Clustering' grouping individual notes
into streams of voices.  This approach is well adapted to follow
arbitrary musical voices, which may wander across registers and also
cross each other.

Currently the algorithm outputs monophonic voices.  Any overlapping
notes are clustered into separate streams, where each note belongs to
one and only one output stream.  A dynamic variable
*overlap-tolerance* holds a factor (0-1.0) to control how forgiving
the algorithm is with respect to what counts as simultaneous.

The feature vectors compared for similarity are built from pitch and
time info in input data, using a function to measure similarity.  The
default distance metrics - #'euclidian-distance - compares pitches
using a mel frequency scale, to do measurements based on perceived
similarity.  The similarity function can be tuned with adjustable
weightings for pitch and time to fit various types of input.

Typical input in OpenMusic is a 'chord-seq'.  Basic output is a list
of chord-seqs, one for each voice found.

Currently, CPU usage grows exponentially with larger input data sets
T(n)= ϴ(n³) (an optimization for HAC based on a Next-Best-Match array
will be implemented when time permits).  If things start to get too
slow select subsets of data to work on, or preferably use the provided
analysis-class: 'stream-seg' to interactively segment data before
applying the stream-separation analysis per segment.

'stream-seg' - a sub-class of OMs 'analysis' class - is provided to
work interactively with stream-segregation within segmentation bounds
of segmented data.  Functions are provided to return the output from
this analysis as a list of lists of chord-seqs, the whole sequence of
analysed segments concatenated into one multi-seq, or data to feed a
Maquette.

Help is available directly from the graphical boxes (hit 'd'), or
through the provided tutorials.

Anders Vinjar - April 2017
