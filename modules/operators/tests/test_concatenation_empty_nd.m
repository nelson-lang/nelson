%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An empty operand next to a nonempty one is dropped when it is 0x0 or when
% both are 2-D. A 2-D empty next to an N-D array, or an N-D empty next to a
% 2-D array, must agree with it, except that a real dense scalar drops an N-D
% empty of its own numeric, logical, char or string class when they are the
% only two operands.
msg = _('Dimensions of arrays being concatenated are not consistent.');
id = 'Nelson:catenate:dimensionMismatch';
%=============================================================================
cmds = {'[zeros(0, 1), NaN(1, 1, 2)]', '[NaN(1, 1, 2), zeros(0, 1)]', ...
        '[zeros(0, 1); NaN(1, 1, 2)]', '[NaN(1, 1, 2); zeros(0, 1)]', ...
        '[zeros(1, 0), NaN(1, 1, 2)]', '[zeros(1, 0); NaN(1, 1, 2)]', ...
        '[zeros(0, 3), ones(2, 2, 2)]', '[zeros(2, 0); ones(1, 2, 2)]', ...
        '[single(zeros(0, 1)), NaN(1, 1, 2)]', '[int8(zeros(0, 1)), int8(ones(1, 1, 2))]', ...
        '[true(0, 1), true(1, 1, 2)]', '[repmat(''a'', 0, 1), repmat(''a'', 1, 1, 2)]', ...
        '[cell(0, 1), cell(1, 1, 2)]', '[strings(0, 1), repmat("a", 1, 1, 2)]', ...
        '[zeros(0, 1), repmat("a", 1, 1, 2)]', ...
        'horzcat(zeros(0, 1), NaN(1, 1, 2))', 'vertcat(NaN(1, 1, 2), zeros(1, 0))', ...
        '[zeros(0, 0, 2), ones(2, 2)]', '[zeros(1, 0, 2), 1:3]', ...
        '[zeros(0, 0, 2), int8(5)]', '[complex(zeros(0, 0, 2)), 1]', ...
        '[zeros(0, 0, 2), 1i]', '[complex(zeros(0, 0, 2)), 1i]', ...
        '[cell(0, 0, 2), {5}]', '[zeros(0, 0, 2), {5}]', ...
        '[zeros(0, 0, 2), 5, 6]', '[5, zeros(0, 0, 2), 6]', ...
        '[zeros(0, 1), zeros(1, 0), NaN(1, 1, 2)]', ...
        '[zeros(0, 1); zeros(1, 0); NaN(1, 1, 2)]', ...
        '[zeros(0, 1), zeros(0, 0, 2), 5]', '[zeros(0, 0, 2), zeros(0, 1), 5]', ...
        'horzcat(zeros(0, 1), zeros(0, 3), NaN(1, 1, 2))', ...
        'cat(3, zeros(0, 1), NaN(1, 1, 2))'};
for k = 1:numel(cmds)
  assert_checkerror(cmds{k}, msg, id);
end
%=============================================================================
% Same rules next to N-D objects, strings and structs, cat included.
addpath(modulepath('operators', 'tests'));
v1 = VertcatOrderValueFixture(1);
vN = cat(3, v1, v1);
h1 = VertcatOrderHandleFixture(1);
hN = cat(3, h1, h1);
sN = cat(3, struct('a', 1), struct('a', 2));
cmds = {'[zeros(0, 1), vN]', '[vN, zeros(0, 1)]', '[zeros(1, 0); vN]', ...
        'cat(3, zeros(0, 1), vN)', '[zeros(0, 1), hN]', '[hN; zeros(1, 0)]', ...
        'cat(3, zeros(0, 1), hN)', '[zeros(0, 0, 2), zeros(0, 3), hN]', ...
        '[zeros(0, 3), zeros(0, 0, 2), vN]', ...
        'cat(3, zeros(0, 1), repmat("a", 1, 1, 2))', ...
        'cat(3, strings(0, 1), repmat("a", 1, 1, 2))', ...
        'cat(3, zeros(0, 1), sN)', '[zeros(0, 1), sN]'};
for k = 1:numel(cmds)
  assert_checkerror(cmds{k}, msg, id);
end
assert_isequal(size([[], vN]), [1 1 2]);
assert_isequal(size([hN; []]), [1 1 2]);
assert_isequal(size(cat(3, [], vN)), [1 1 2]);
assert_isequal(size([zeros(0, 1), v1]), [1 1]);
assert_isequal(size([h1; zeros(3, 0)]), [1 1]);
%=============================================================================
% 0x0 is always dropped, 2-D empties next to 2-D arrays too.
R = [[], NaN(1, 1, 2)];
assert_isequal(size(R), [1 1 2]);
R = [NaN(1, 1, 2); single([])];
assert_isequal(size(R), [1 1 2]);
assert_isequal(class(R), 'single');
R = [[], NaN(1, 1, 2), []];
assert_isequal(size(R), [1 1 2]);
R = [zeros(0, 1), ones(2, 2)];
assert_isequal(R, ones(2, 2));
R = [zeros(0, 1), zeros(1, 0), 5];
assert_isequal(R, 5);
%=============================================================================
% An N-D empty that agrees with the other operands is kept.
R = [zeros(1, 0, 2), NaN(1, 1, 2)];
assert_isequal(size(R), [1 1 2]);
R = [zeros(1, 0, 2), NaN(1, 1, 2), zeros(1, 0, 2)];
assert_isequal(size(R), [1 1 2]);
R = [zeros(0, 1, 2); NaN(1, 1, 2); zeros(0, 1, 2)];
assert_isequal(size(R), [1 1 2]);
%=============================================================================
% A real dense scalar drops an N-D empty of its own class.
assert_isequal([zeros(0, 0, 2), 5], 5);
assert_isequal([5; zeros(1, 0, 2)], 5);
assert_isequal([single(zeros(0, 0, 2)), single(5)], single(5));
assert_isequal([int8(zeros(0, 1, 2)); int8(5)], int8(5));
assert_isequal([true(0, 0, 2), true], true);
assert_isequal([repmat('a', 0, 0, 2), 'b'], 'b');
assert_isequal([strings(0, 0, 2), "b"], "b");
assert_isequal([zeros(0, 0, 2), "b"], "b");
%=============================================================================
% Every operand empty: the empty concatenation size rule applies.
R = [zeros(0, 1), zeros(0, 0, 2)];
assert_isequal(size(R), [0 0]);
%=============================================================================
