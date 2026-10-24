%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricyle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
E = entries(d);
asserts.isequal(class(E), 'table');
asserts.isequal(E.Properties.VariableNames, {'Key', 'Value'});
asserts.isequal(E.Key, [1; 2; 3]);
asserts.isequal(E.Value, ["Unicycle"; "Bicycle"; "Tricyle"]);
%=============================================================================
R = entries(d, 'cell');
REF = {1, "Unicycle"; 2, "Bicycle"; 3, "Tricyle"};
asserts.isequal(R, REF);
%=============================================================================
R = entries(d, 'struct');
asserts.isequal(R(1), struct('Key', 1, 'Value', "Unicycle"));
asserts.isequal(R(2), struct('Key', 2, 'Value', "Bicycle"));
asserts.isequal(R(3), struct('Key', 3, 'Value', "Tricyle"));
%=============================================================================
d = dictionary(string({'scalar', 'vector', 'cell'}), {1, [2 3], {4, 5}});
R = entries(d, 'cell');
asserts.isequal(R{1, 2}, 1);
asserts.isequal(R{2, 2}, [2 3]);
asserts.isequal(R{3, 2}, {4, 5});
R = entries(d, 'table');
asserts.isequal(R.Value{1}, 1);
asserts.isequal(R.Value{2}, [2 3]);
asserts.isequal(R.Value{3}, {4, 5});
%=============================================================================
