# IIMB

The IIMB task is focused on two main goals:

  1. to provide an evaluation data set for various kinds of data transformations, including
     value transformations, structural transformations and logical transformations;
  2. to cover a wide spectrum of possible techniques and tools.

ISLab Instance Matching Benchmark (IIMB), that has been generated using the
SWING tool. Participants were requested to find the correct correspondences
among individuals of the first knowledge base and individuals of the other
one. An important task here is that some of the transformations require
automatic reasoning for finding the expected alignments.

IIMB is composed of a set of test cases, each one represented by a set of
instances, i.e., an OWL ABox, built from an initial data set of real linked
data extracted from the Web. Then, the ABox is automatically modified in
several ways by generating a set of new ABoxes, called test cases. Each test
case is produced by transforming the individual descriptions in the
reference ABox in new individual descriptions that are inserted in the test
case at hand. The goal of transforming the original individuals is twofold:
on one side, we provide a simulated situation where data referring to the
same objects are provided in different data sources; on the other side, we
generate different data sets with a variable level of data quality and
complexity. IIMB provides transformation techniques supporting modifications
of data property values, modifications of number and type of properties used
for the individual description, and modifications of the individuals
classification. The first kind of transformations is called *data value
transformation* and it aims at simulating the fact that data expressing the
same real object in different data sources may be different because of data
errors or because of the usage of different conventional patterns for data
representation. The second kind of transformations is called *data structure
transformation* and it aims at simulating the fact that the same real object
may be described using different properties/attributes in different data
sources. Finally, the third kind of transformations, called *data semantic
transformation*, simulates the fact that the same real object may be
classified in different ways in different data sources.

The 2012 edition has been created by exploiting the same OWL source used
for the Sandbox task. The main difference is that we introduced in IIMB
a large set of data transformations. In particular, test cases from 0 to
20 contain changes in data format (misspelling, errors in text, etc);
test cases 21 to 40 contain changes in structure (properties missing,
RDF triples changed); 41 to 60 contain logical changes (class membership
changed, logical errors); finally, test cases 61 to 80 contain a mix of
the previous.

Mismatch types:

  - Data value transformations
  - Data structure transformation
  - Data semantic transformation
