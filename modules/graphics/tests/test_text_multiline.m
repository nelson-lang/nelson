%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
reset(groot());
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
% Reference boxes for one line of each width, to compare the stacked block to.
short = text(ax, 0.1, 0.1, 'first line');
long = text(ax, 0.1, 0.2, 'second line');
drawnow();
shortExtent = get(short, 'Extent');
longExtent = get(long, 'Extent');
asserts.istrue(longExtent(3) > shortExtent(3));
%=============================================================================
% A char(10) in the string breaks the line: the block is as wide as its widest
% line (not the two of them run together) and taller than a single line.
tNewline = text(ax, 0.1, 0.3, sprintf('first line\nsecond line'));
drawnow();
newlineExtent = get(tNewline, 'Extent');
asserts.isequal(class(get(tNewline, 'String')), 'char');
asserts.isequal(numel(get(tNewline, 'String')), 22);
asserts.isapprox(newlineExtent(3), longExtent(3));
asserts.istrue(newlineExtent(4) > longExtent(4));
asserts.istrue(newlineExtent(3) < shortExtent(3) + longExtent(3));
%=============================================================================
% The '|' delimiter keeps working and lays out the same block.
tPipe = text(ax, 0.1, 0.4, 'first line|second line');
drawnow();
pipeExtent = get(tPipe, 'Extent');
asserts.isequal(get(tPipe, 'String'), {'first line'; 'second line'});
asserts.isapprox(pipeExtent(3), newlineExtent(3));
asserts.isapprox(pipeExtent(4), newlineExtent(4));
%=============================================================================
% A cell of character vectors renders as as many lines, not as one string.
tCell = text(ax, 0.1, 0.5, {'first line', 'second line'});
drawnow();
cellExtent = get(tCell, 'Extent');
asserts.isapprox(cellExtent(3), newlineExtent(3));
asserts.isapprox(cellExtent(4), newlineExtent(4));
%=============================================================================
% A character matrix is accepted and gives one line per row.
tCharMatrix = text(ax, 0.1, 0.6, ['first line '; 'second line']);
drawnow();
charMatrixExtent = get(tCharMatrix, 'Extent');
asserts.isequal(get(tCharMatrix, 'String'), {'first line'; 'second line'});
asserts.isapprox(charMatrixExtent(3), newlineExtent(3));
asserts.isapprox(charMatrixExtent(4), newlineExtent(4));
%=============================================================================
% Three lines are taller than two, by the same step two is taller than one.
tThree = text(ax, 0.1, 0.7, sprintf('first line\nsecond line\nfirst line'));
drawnow();
threeExtent = get(tThree, 'Extent');
asserts.istrue(threeExtent(4) > newlineExtent(4));
asserts.isapprox(threeExtent(4) - newlineExtent(4), newlineExtent(4) - longExtent(4), 1e-6);
asserts.isapprox(threeExtent(3), newlineExtent(3));
%=============================================================================
% A string array is another way of writing the same block.
tString = text(ax, 0.1, 0.8, ["first line"; "second line"]);
drawnow();
stringExtent = get(tString, 'Extent');
asserts.isapprox(stringExtent(3), newlineExtent(3));
asserts.isapprox(stringExtent(4), newlineExtent(4));
%=============================================================================
% A character matrix with padded rows loses the padding, as cellstr does.
tPadded = text(ax, 0.1, 0.9, ['ab '; 'cde']);
asserts.isequal(get(tPadded, 'String'), {'ab'; 'cde'});
%=============================================================================
% A value that is none of those forms is still rejected.
R = false;
try
  set(tPadded, 'String', magic(3));
catch e
  R = strcmp(e.identifier, 'Nelson:graphics:propertyError');
end
asserts.istrue(R);
%=============================================================================
close(f);
close all;
%=============================================================================
