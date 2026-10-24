%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% stream2 and stream3 compute streamline vertices without drawing anything.
% They share the tracer with streamline, so streamline(stream2(...)) and
% streamline(X, Y, U, V, sx, sy) must follow exactly the same path.
%=============================================================================
assert_isequal(nargin('stream2'), -1);
assert_isequal(nargin('stream3'), -1);
%=============================================================================
[X, Y] = meshgrid(0:10, 0:10);
U = ones(size(X));
V = zeros(size(X));
%=============================================================================
% Nothing is drawn: no figure is opened.
nbFigures = numel(findall(0, 'Type', 'figure'));
vertices = stream2(X, Y, U, V, 1, 5);
assert_isequal(numel(findall(0, 'Type', 'figure')), nbFigures);
%=============================================================================
% One row cell per start point, each an N-by-2 double starting on its point.
assert_istrue(iscell(vertices));
assert_isequal(size(vertices), [1 1]);
assert_istrue(isa(vertices{1}, 'double'));
assert_isequal(size(vertices{1}), [91 2]);
assert_isequal(vertices{1}(1, :), [1 5]);
%=============================================================================
% The step defaults to a tenth of a grid cell, and the cell here is 1 wide.
% The subtraction of two traced coordinates is not exact, hence the tolerance.
assert_isapprox(vertices{1}(2, :) - vertices{1}(1, :), [0.1 0], 1e-12);
%=============================================================================
vertices = stream2(X, Y, U, V, [1 2], [5 6]);
assert_isequal(size(vertices), [1 2]);
assert_isequal(vertices{1}(1, :), [1 5]);
assert_isequal(vertices{2}(1, :), [2 6]);
%=============================================================================
% Without a grid the field is indexed from 1, so the line is one cell longer.
vertices = stream2(U, V, 1, 5);
assert_isequal(vertices{1}(1, :), [1 5]);
assert_isequal(size(vertices{1}, 1), 101);
%=============================================================================
% options is [stepsize] or [stepsize, maxvert], the step counted in grid cells.
vertices = stream2(X, Y, U, V, 1, 5, 0.5);
assert_isequal(size(vertices{1}, 1), 19);
assert_isapprox(vertices{1}(2, :) - vertices{1}(1, :), [0.5 0], 1e-12);
%=============================================================================
vertices = stream2(X, Y, U, V, 1, 5, 1.0);
assert_isequal(size(vertices{1}, 1), 10);
assert_isapprox(vertices{1}(2, :) - vertices{1}(1, :), [1 0], 1e-12);
%=============================================================================
% maxvert counts the start point in.
vertices = stream2(X, Y, U, V, 1, 5, [0.1 3]);
assert_isequal(size(vertices{1}, 1), 3);
vertices = stream2(X, Y, U, V, 1, 5, [0.1 10]);
assert_isequal(size(vertices{1}, 1), 10);
%=============================================================================
% A start point outside the field traces nothing at all.
vertices = stream2(X, Y, U, V, -5, -5);
assert_istrue(isempty(vertices{1}));
%=============================================================================
% A start point the field does not move keeps its single vertex.
Z = zeros(size(X));
vertices = stream2(X, Y, Z, Z, 5, 5);
assert_isequal(size(vertices{1}, 1), 1);
assert_isequal(vertices{1}, [5 5]);
%=============================================================================
% stream3 answers the same way with a third column.
[X3, Y3, Z3] = meshgrid(0:5, 0:5, 0:5);
U3 = ones(size(X3));
V3 = zeros(size(X3));
W3 = zeros(size(X3));
vertices = stream3(X3, Y3, Z3, U3, V3, W3, 1, 2, 3);
assert_istrue(iscell(vertices));
assert_isequal(size(vertices), [1 1]);
assert_isequal(size(vertices{1}), [41 3]);
assert_isequal(vertices{1}(1, :), [1 2 3]);
%=============================================================================
vertices = stream3(U3, V3, W3, 1, 2, 3);
assert_isequal(vertices{1}(1, :), [1 2 3]);
assert_isequal(size(vertices{1}, 1), 51);
%=============================================================================
% streamline traces the same path whether it is handed the field or the
% vertices computed from it.
[X, Y] = meshgrid(-2:0.25:2, -2:0.25:2);
U = -Y;
V = X;
f = figure('Visible', 'off');
h = streamline(X, Y, U, V, 1, 0);
direct = [h(1).XData', h(1).YData'];
close(f);
f = figure('Visible', 'off');
h = streamline(stream2(X, Y, U, V, 1, 0));
throughStream2 = [h(1).XData', h(1).YData'];
close(f);
assert_isequal(direct, throughStream2);
%=============================================================================
[X3, Y3, Z3] = meshgrid(-2:0.5:2, -2:0.5:2, -2:0.5:2);
U3 = -Y3;
V3 = X3;
W3 = 0.2 * ones(size(X3));
f = figure('Visible', 'off');
h = streamline(X3, Y3, Z3, U3, V3, W3, 1, 0, -1);
direct = [h(1).XData', h(1).YData', h(1).ZData'];
close(f);
f = figure('Visible', 'off');
h = streamline(stream3(X3, Y3, Z3, U3, V3, W3, 1, 0, -1));
throughStream3 = [h(1).XData', h(1).YData', h(1).ZData'];
close(f);
assert_isequal(direct, throughStream3);
%=============================================================================
% The options argument reaches streamline too, and no longer goes unread.
f = figure('Visible', 'off');
h = streamline(X, Y, U, V, 1, 0, [0.1 7]);
assert_isequal(numel(h(1).XData), 7);
close(f);
%=============================================================================
% Neither takes a line property: they draw nothing to apply it to.
assert_checkerror('stream2(ones(3), ones(3), 1, 1, ''Color'', ''r'')', getString(message('Nelson:graphics:unexpectedPropertyArgument')));
%=============================================================================
assert_checkerror('stream2(1, 2)', getString(message('Nelson:narginchk:notEnoughInputs')));
assert_checkerror('stream3(1, 2, 3, 4, 5)', getString(message('Nelson:narginchk:notEnoughInputs')));
%=============================================================================
% A three dimensional field given to stream2 is refused, and the other way.
assert_checkerror('stream2(ones(3, 3, 3), ones(3, 3, 3), ones(3, 3, 3), 1, 1, 1)', getString(message('Nelson:stream2:twoDimensionalFieldExpected')));
%=============================================================================
