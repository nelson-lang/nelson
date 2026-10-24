%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: vectorized text operations over a 1000-element cellstr.
% - strcmp against a char scalar compares the raw charType buffers,
%   no per-element wide-string conversion
% - regexprep/regexp with a single shared pattern compile (and JIT) it once
%   instead of once per element
words = cell(1, 1000);
for i = 1:1000
  words{i} = sprintf('item_%d_suffix', i);
end
%=============================================================================
R = 50;
tic();
for r = 1:R
  n = sum(strcmp(words, 'item_500_suffix'));
end
t_strcmp = toc();
asserts.isequal(n, 1);
fprintf('  strcmp cellstr vs scalar: %0.1f us/call\n', t_strcmp / R * 1e6);
asserts.istrue(t_strcmp / R < 0.5e-3);
%=============================================================================
tic();
for r = 1:R
  eq = strcmp(words, words);
end
t_strcmp2 = toc();
asserts.istrue(all(eq));
fprintf('  strcmp cellstr/cellstr:   %0.1f us/call\n', t_strcmp2 / R * 1e6);
%=============================================================================
R2 = 10;
tic();
for r = 1:R2
  w2 = regexprep(words, '_[0-9]+_', '-');
end
t_rep = toc();
asserts.isequal(w2{500}, 'item-suffix');
fprintf('  regexprep shared pattern: %0.2f ms/call\n', t_rep / R2 * 1e3);
asserts.istrue(t_rep / R2 < 25e-3);
%=============================================================================
tic();
for r = 1:R2
  m = regexp(words, '[0-9]+', 'match', 'once');
end
t_match = toc();
asserts.isequal(m{500}, '500');
fprintf('  regexp shared pattern:    %0.2f ms/call\n', t_match / R2 * 1e3);
%=============================================================================
% behavior checks
asserts.isequal(strcmp({'a', 'ab', 'b'}, 'ab'), [false, true, false]);
asserts.isequal(strcmp({'a', 'b'}, {'a', 'c'}), [true, false]);
asserts.isequal(strcmp({'a', 1}, 'a'), [true, false]);
asserts.istrue(isempty(strcmp(cell(1, 0), 'a')));
asserts.isequal(regexprep({'a1', 'b22'}, '[0-9]+', 'X'), {'aX', 'bX'});
asserts.isequal(regexprep({'a1', 'b22'}, {'1', '22'}, 'X'), {'aX', 'bX'});
%=============================================================================
