% A text sample used to the test the Huffman coding and decoding.
sample("I used to analyze myself down to the last thread, used to compare myself with others, recalled all the smallest glances, smiles and words of those to whom I’d tried to be frank, interpreted everything in a bad light, laughed viciously at my attempts ‘to be like the rest’ –and suddenly, in the midst of my laughing, I’d give way to sadness, fall into ludicrous despondency and once again start the whole process all over again – in short, I went round and round like a squirrel on a wheel.").

% letter_frequency(L1, L2) :- the list L2 is obtained from the list L1
% by counting the frequency of each unique character code in L1.
% Consecutive duplicates of elements are stored as terms [C, N] where N
% is the number of duplicates of the element C.
% (list, list) (+, ?)
letter_frequency(Text, Freq) :-
    letter_frequency(Text, [], Freq).
letter_frequency([], Acc, Acc).
letter_frequency([UElement|Text], Acc, Freq) :-
    letter_frequency_helper(Text, UElement, 1, Count, RemText),
    letter_frequency(RemText, [[UElement, Count]|Acc], Freq).

letter_frequency_helper([Element|Text], Element, Acc, Count, RemText) :-
    Acc1 is Acc+1,
    letter_frequency_helper(Text, Element, Acc1, Count, RemText).
letter_frequency_helper(RemText, _, Acc, Acc, RemText).

% freq_to_leaves(L1, T2) :- the flat tree L2 is obtained from the list
% L1 by converting each element in L1 to a leaf node and storing it in
% L2. The elements in L1 should be sorted in character frequency in
% ascending order.
% (list, tree) (+, ?)
freq_to_leaves(Frequency, Leaves) :-
    freq_to_leaves(Frequency, [], Leaves).
freq_to_leaves([], Acc, Acc).
freq_to_leaves([[Letter, Count]|Frequency], Acc, Leaves) :-
    freq_to_leaves(Frequency, [tree(leaf, Letter, Count, na, na)|Acc], Leaves).

% construct_tree(T1, T2) :- the Huffman tree T1 is constructed by
% recursively summarizing the first two nodes E1 and E2 in the T1 and
% appending the summary of E1 and E2's frequency as a node E3 =
% tree(node, na, FreqSum, E1, E2) to T1. T2 is used to return the
% result through unification.
% (tree, tree) (+, ?)
construct_tree([Huffman], Huffman).
construct_tree([tree(Type1, Letter1, Freq1, Left1, Right1), tree(Type2, Letter2, Freq2, Left2, Right2)|Leaves], Tree) :-
    Freq3 is Freq1+Freq2,
    construct_tree([tree(node, na, Freq3, tree(Type1, Letter1, Freq1, Left1, Right1), tree(Type2, Letter2, Freq2, Left2, Right2))|Leaves], Tree).

% huffman_code(T1, C) :- Traverses the Huffman tree once by going
% down the left branches until a leaf node is found. At each branch, a 0
% or 1 is added to an accumulator depending on the branch direction.
% When a leaf node is encountered, the term [Letter, Code] is returned
% in C.
% (tree, list) (+, ?)
huffman_code(Tree, Codes) :-
    huffman_code(Tree, [], Codes).
huffman_code(na, Acc, Acc).
huffman_code(tree(leaf, Letter, _, na, na), Acc, Codes) :-
    reverse(Acc, Acc1), %To get the Huffman code in the right order
    huffman_code(na, [Letter, Acc1], Codes).
huffman_code(tree(node, na, _, Left, _), Acc, Codes) :-
    huffman_code(Left, [0|Acc], Codes).
huffman_code(tree(node, na, _, _, Right), Acc, Codes) :-
    huffman_code(Right, [1|Acc], Codes).

% encode(T, C, E) :- the input Ascii Text T is encoded by recursively
% going through each character code in T and searching for a matching
% code in the Huffman codes C. The Huffman code is then front appended
% to an accumulator which eventually returns an encoded text in E.
% (list, list, list) (+, +, ?)
encode(Text, HuffmanCode, Encoded) :-
    reverse(Text, ReversedText),
    encode(ReversedText, HuffmanCode, [], Encoded).
encode([], _, Acc, Acc).
encode([Letter|Text], HuffmanCode, Acc, Encoded) :-
    search_letter(Letter, HuffmanCode, Code),
    append(Code, Acc, Acc1),
    encode(Text, HuffmanCode, Acc1, Encoded).

% decode(E, C, A) :- the input encoded text E is decoded by trying to
% decode an increasing buffer of the first bits in E using the Huffman
% codes in C. If the bits are succesfully decoded the buffer is emptied
% and the decoded character is added to the decoded text A. If no
% character is found for the bits another bit from the encoded text is
% added to the buffer and searched for in C.
% (list, list, list) (+, +, ?)
decode(Encoded, HuffmanCode, Ascii) :-
    decode(Encoded, HuffmanCode, [], [], ReversedAscii),
    reverse(ReversedAscii, Ascii).
decode([], _, _, Acc, Acc).
decode([Bit|Encoded], HuffmanCode, Buffer, Acc, Ascii) :-
    append(Buffer, [Bit], Buffer1), %Otherwise we check for reversed letters lol
    (   search_letter(Letter, HuffmanCode, Buffer1) -> decode(Encoded, HuffmanCode, [], [Letter|Acc], Ascii) ; decode(Encoded, HuffmanCode, Buffer1, Acc, Ascii)).

% search_letter(E, C, H) :- A bidirectional predicate which returns
% either the decoded character E or the Huffman code H for a character
% by searching through the Huffman codes C. Returns false if the
% character or the code cannot be found.
% (atom, list, list) (?, +, ?)
search_letter(Letter, [[Letter, Code]|_], Code).
search_letter(Letter, [_|HuffmanCode], Code) :-
    search_letter(Letter, HuffmanCode, Code).

% test() :- Test predicate which runs the encoding and decoding of a
% sample text with prints of each step in the process.
test() :-
    sample(Text),
    string_to_list(Text, Ascii),
    msort(Ascii, SortedAscii), %Does not remove duplicates

    letter_frequency(SortedAscii, Freq),
    freq_to_leaves(Freq, Leaves),
    sort(3, @=<, Leaves, SortedLeaves), %Sort the leaves by frequency
    construct_tree(SortedLeaves, Tree),
    findall(Codes, huffman_code(Tree, Codes), HuffmanCode),
    encode(Ascii, HuffmanCode, Encoded),
    decode(Encoded, HuffmanCode, Decoded),
    string_to_list(EncodedDecoded, Decoded),

    write("Original Text: "), write(Text), nl, nl,
    write("Ascii: "), write(Ascii), nl, nl,
    write("Letter Frequencies: "), write(Freq), nl, nl,
    write("Sorted Huffman Leaves: "), write(SortedLeaves), nl, nl,
    write("Huffman Tree: "), write(Tree), nl, nl,
    write("Huffman Codes: "), write(HuffmanCode), nl, nl,
    write("Encoded Ascii: "), write(Encoded), nl, nl,
    write("Decoded Ascii: "), write(Decoded), nl, nl,
    write("Decoded Text: "), write(EncodedDecoded), nl, nl.

% test(T, E) :- A test predicate which takes an input text T, generates
% Huffman codes and encodes the text, returning it in E.
% (atom, list) (+, ?)
test(Input, EncodedText) :-
    string_to_list(Input, Ascii),
    msort(Ascii, SortedAscii), %Does not remove duplicates

    letter_frequency(SortedAscii, Freq),
    freq_to_leaves(Freq, Leaves),
    sort(3, @=<, Leaves, SortedLeaves), %Sort the leaves by frequency
    construct_tree(SortedLeaves, Tree),
    findall(Codes, huffman_code(Tree, Codes), HuffmanCode),

    write("Original Text: "), write(Input), nl, nl,
    write("Ascii: "), write(Ascii), nl, nl,
    write("Letter Frequencies: "), write(Freq), nl, nl,
    write("Sorted Huffman Leaves: "), write(SortedLeaves), nl, nl,
    write("Huffman Tree: "), write(Tree), nl, nl,
    write("Huffman Codes: "), write(HuffmanCode), nl, nl,

    encode(Ascii, HuffmanCode, EncodedText).
