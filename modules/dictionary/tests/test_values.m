%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
R = values(d);
REF = ["Unicycle"; "Bicycle"; "Tricycle"];
asserts.isequal(R, REF);
%=============================================================================
d = dictionary(string({'scalar', 'vector', 'cell'}), {1, [2 3], {4, 5}});
R = values(d, 'cell');
asserts.isequal(R{1}, 1);
asserts.isequal(R{2}, [2 3]);
asserts.isequal(R{3}, {4, 5});
R = values(d);
asserts.isequal(numel(R), 3);
asserts.isequal(R{1}, 1);
asserts.isequal(R{2}, [2 3]);
asserts.isequal(R{3}, {4, 5});
%=============================================================================
