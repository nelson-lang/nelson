%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% Regression: table data-linking source properties (XVariable, YVariable,
% ZVariable, SizeVariable, ColorVariable, AlphaVariable, RVariable,
% ThetaVariable, GroupVariable) must be char with an empty-string default,
% not an empty double. image MaxRenderedResolution and stackedplot
% MarkerFaceColor must be char ('none'), not double.
%=============================================================================
xyz = {'XVariable', 'YVariable', 'ZVariable'};
xyzsca = {'XVariable', 'YVariable', 'ZVariable', 'SizeVariable', ...
  'ColorVariable', 'AlphaVariable'};
rtheta = {'RVariable', 'ThetaVariable'};
%=============================================================================
h = scatter(1:5, 1:5);
assert_char_empty(h, xyzsca);
close('all');
%=============================================================================
h = scatter3(1:5, 1:5, 1:5);
assert_char_empty(h, xyzsca);
close('all');
%=============================================================================
h = swarmchart(ones(1, 10), 1:10);
assert_char_empty(h, xyzsca);
close('all');
%=============================================================================
h = bubblechart(1:5, 1:5, 1:5);
assert_char_empty(h, xyzsca);
close('all');
%=============================================================================
h = stem(1:5);
assert_char_empty(h, xyz);
close('all');
%=============================================================================
h = polarplot(1:10, 1:10);
assert_char_empty(h, rtheta);
close('all');
%=============================================================================
h = compassplot(1 + 1i);
assert_char_empty(h, rtheta);
close('all');
%=============================================================================
h = parallelplot(magic(4));
assert_char_empty(h, {'GroupVariable'});
close('all');
%=============================================================================
h = image(magic(4));
value = get(h, 'MaxRenderedResolution');
asserts.isequal(class(value), 'char');
asserts.isequal(value, 'none');
close('all');
%=============================================================================
h = stackedplot([1 2; 3 4; 5 6]);
value = get(h, 'MarkerFaceColor');
asserts.isequal(class(value), 'char');
asserts.isequal(value, 'none');
close('all');
%=============================================================================
function assert_char_empty(h, names)
  for k = 1:numel(names)
    name = names{k};
    value = get(h, name);
    asserts.isequal(class(value), 'char');
    asserts.isequal(value, '');
  end
end
%=============================================================================
