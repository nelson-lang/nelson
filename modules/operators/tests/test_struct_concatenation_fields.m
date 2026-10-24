%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Structs concatenate when they have the same set of fields, in any order.
%=============================================================================
id = 'Nelson:catenate:structFieldBad';
namesMsg = _('Names of fields in structure arrays being concatenated do not match. Concatenation of structure arrays requires that these arrays have the same set of fields.');
numberMsg = _('Number of fields in structure arrays being concatenated do not match. Concatenation of structure arrays requires that these arrays have the same set of fields.');
%=============================================================================
asserts.checkerror('[struct(''a'', 1), struct(''b'', 1)]', namesMsg, id);
asserts.checkerror('[struct(''a'', 1); struct(''b'', 1)]', namesMsg, id);
asserts.checkerror('horzcat(struct(''a'', 1), struct(''b'', 1))', namesMsg, id);
asserts.checkerror('vertcat(struct(''a'', 1), struct(''b'', 1))', namesMsg, id);
asserts.checkerror('cat(3, struct(''a'', 1), struct(''b'', 1))', namesMsg, id);
asserts.checkerror('[struct(''a'', 1), struct(''A'', 1)]', namesMsg, id);
asserts.checkerror('[struct(''a'', 1, ''b'', 1), struct(''a'', 1, ''c'', 1)]', namesMsg, id);
%=============================================================================
asserts.checkerror('[struct(''a'', 1), struct(''a'', 1, ''b'', 2)]', numberMsg, id);
asserts.checkerror('[struct(''a'', 1, ''b'', 2), struct(''a'', 1)]', numberMsg, id);
asserts.checkerror('[struct(), struct(''b'', 2)]', numberMsg, id);
%=============================================================================
% the first mismatching operand, compared to the first struct, names the error
asserts.checkerror('[struct(''a'', 1), struct(''a'', 2), struct(''b'', 3)]', namesMsg, id);
asserts.checkerror('[struct(''a'', 1), struct(''b'', 1), struct(''a'', 1, ''b'', 1)]', namesMsg, id);
asserts.checkerror('[struct(''a'', 1, ''b'', 1), struct(''b'', 1), struct(''a'', 1)]', numberMsg, id);
asserts.checkerror('cat(3, struct(''a'', 1), struct(''a'', 1), struct(''b'', 1))', namesMsg, id);
%=============================================================================
% an empty struct with other fields still conflicts
asserts.checkerror('[struct(''b'', 2), struct(''a'', cell(0, 1))]', namesMsg, id);
asserts.checkerror('[struct(''a'', cell(0, 1)), struct(''b'', 2)]', namesMsg, id);
asserts.checkerror('[struct(''a'', {}), struct(''b'', {})]', namesMsg, id);
asserts.checkerror('[struct(''a'', {}), struct(''a'', 1, ''b'', 2)]', numberMsg, id);
asserts.checkerror('[struct(''b'', 2), repmat(struct(), 1, 0)]', numberMsg, id);
asserts.checkerror('cat(1, struct(''a'', {}), struct(''b'', {}))', namesMsg, id);
asserts.checkerror('cat(3, struct(''a'', {}), struct(''b'', {}))', namesMsg, id);
asserts.checkerror('[struct(''b'', 2), {}, struct(''a'', cell(0, 1))]', namesMsg, id);
asserts.checkerror('[struct(''a'', 1), {}, struct(''b'', 2)]', namesMsg, id);
%=============================================================================
% a 0x0 struct without fields is dropped
R = [struct('b', 2), struct([])];
asserts.isequal(fieldnames(R), {'b'});
R = [struct([]), struct('b', 2)];
asserts.isequal(R, struct('b', 2));
%=============================================================================
% field order does not matter: the first struct gives it
R = [struct('a', 1, 'b', 2), struct('b', 3, 'a', 4)];
asserts.isequal(size(R), [1 2]);
asserts.isequal(fieldnames(R), {'a'; 'b'});
asserts.isequal([R.a], [1 4]);
asserts.isequal([R.b], [2 3]);
R = [struct('a', 1, 'b', 2); struct('b', 3, 'a', 4)];
asserts.isequal(size(R), [2 1]);
asserts.isequal([R.a], [1 4]);
R = cat(3, struct('a', 1, 'b', 2), struct('b', 3, 'a', 4));
asserts.isequal(size(R), [1 1 2]);
asserts.isequal([R.a], [1 4]);
R = [struct('a', 1, 'b', 2, 'c', 3), struct('c', 1, 'a', 2, 'b', 3), struct('b', 1, 'c', 2, 'a', 3)];
asserts.isequal(fieldnames(R), {'a'; 'b'; 'c'});
asserts.isequal([R.a], [1 2 3]);
asserts.isequal([R.c], [3 1 2]);
R = [struct('b', {}, 'a', {}), struct('a', 1, 'b', 2)];
asserts.isequal(fieldnames(R), {'b'; 'a'});
asserts.isequal(R.a, 1);
R = [struct('a', 1, 'b', 2), struct('b', {}, 'a', {})];
asserts.isequal(fieldnames(R), {'a'; 'b'});
%=============================================================================
% nested structs are values
R = [struct('a', struct('x', 1)), struct('a', struct('y', 1))];
asserts.isequal(size(R), [1 2]);
%=============================================================================
% empty structs keep their fields
R = [struct('a', {}), struct('a', {})];
asserts.isequal(size(R), [0 0]);
asserts.isequal(fieldnames(R), {'a'});
R = [struct('a', cell(0, 1)), struct('a', cell(0, 1))];
asserts.isequal(size(R), [0 2]);
asserts.isequal(fieldnames(R), {'a'});
R = [struct('a', cell(1, 0)); struct('a', cell(1, 0))];
asserts.isequal(size(R), [2 0]);
asserts.isequal(fieldnames(R), {'a'});
R = [struct('a', {}, 'b', {}), struct('b', cell(0, 1), 'a', cell(0, 1))];
asserts.isequal(size(R), [0 1]);
asserts.isequal(fieldnames(R), {'a'; 'b'});
R = [[], struct('a', cell(0, 1))];
asserts.isequal(size(R), [0 1]);
asserts.isequal(fieldnames(R), {'a'});
R = [struct([]), struct('a', cell(0, 1))];
asserts.isequal(fieldnames(R), {'a'});
R = cat(3, struct('a', {}, 'b', {}), struct('b', {}, 'a', {}));
asserts.isequal(size(R), [0 0 2]);
asserts.isequal(fieldnames(R), {'a'; 'b'});
%=============================================================================
% a struct before any cell makes a struct: the empty cells are dropped
R = [struct('b', 2), {}];
asserts.isequal(R, struct('b', 2));
R = [struct('a', 1), {}, {}];
asserts.isequal(R, struct('a', 1));
R = [struct('a', 1), cell(0, 1)];
asserts.isequal(R, struct('a', 1));
R = [struct('a', 1); cell(1, 0)];
asserts.isequal(R, struct('a', 1));
R = [struct('a', 1), {}, struct('a', 2)];
asserts.isequal([R.a], [1 2]);
R = [struct('a', 1); {}; struct('a', 2)];
asserts.isequal(size(R), [2 1]);
R = cat(3, struct('a', 1), {}, struct('a', 2));
asserts.isequal(size(R), [1 1 2]);
R = [struct('a', cell(0, 1)), {}];
asserts.isequal(size(R), [0 1]);
asserts.isequal(fieldnames(R), {'a'});
R = [struct('a', cell(0, 1)), {}, struct('a', cell(0, 1))];
asserts.isequal(size(R), [0 2]);
asserts.istrue(isstruct(R));
msg = sprintf(_('The following error occurred converting from %s to %s:\n%s'), 'cell', 'struct', sprintf(_('Conversion to struct from %s is not possible.'), 'cell'));
asserts.checkerror('[struct(''a'', 1), {1}]', msg, 'Nelson:UnableToConvert');
msg = sprintf(_('The following error occurred converting from %s to %s:\n%s'), 'double', 'struct', sprintf(_('Conversion to struct from %s is not possible.'), 'double'));
asserts.checkerror('[struct(''a'', 1), {}, 1]', msg, 'Nelson:UnableToConvert');
asserts.checkerror('[1, struct(''a'', 1), {}]', msg, 'Nelson:UnableToConvert');
%=============================================================================
% a cell before any struct makes a cell
R = [{}, struct('b', 2)];
asserts.istrue(iscell(R));
asserts.isequal(R, {struct('b', 2)});
R = [[], {}, struct('a', 1)];
asserts.isequal(R, {struct('a', 1)});
R = [{}, 1, struct('a', 1)];
asserts.isequal(R, {1, struct('a', 1)});
R = [{}, struct('a', 1), struct('b', 2)];
asserts.isequal(size(R), [1 2]);
asserts.istrue(iscell(R));
%=============================================================================
