%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
U = T;
T.Properties.VariableUnits = {'m', 'm'};
U.Properties.VariableUnits = {'', 'm'};
%=============================================================================
lastwarn('');
R = T + U;
asserts.isequal(R.Properties.VariableUnits, {'m', 'm'});
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:table:math:UnitWarning');
%=============================================================================
R = T .* T;
asserts.isequal(R.Properties.VariableUnits, {});
lastwarn('');
R = T .* U;
asserts.isequal(R.Properties.VariableUnits, {'m', ''});
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:table:math:UnitWarning');
%=============================================================================
lastwarn('');
R = T ./ U;
asserts.isequal(R.Properties.VariableUnits, {'m', ''});
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:table:math:UnitWarning');
lastwarn('');
R = U ./ T;
asserts.isequal(R.Properties.VariableUnits, {});
[~, id] = lastwarn();
asserts.isequal(id, '');
%=============================================================================
lastwarn('');
R = T .\ U;
asserts.isequal(R.Properties.VariableUnits, {'m', ''});
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:table:math:UnitWarning');
lastwarn('');
R = U .\ T;
asserts.isequal(R.Properties.VariableUnits, {});
[~, id] = lastwarn();
asserts.isequal(id, '');
%=============================================================================
R = T .^ 2;
asserts.isequal(R.Properties.VariableUnits, {});
asserts.checkerror('T .^ T', _('Table variable units must be compatible.'));
%=============================================================================
R = abs(T);
asserts.isequal(R.Properties.VariableUnits, {'m', 'm'});
R = sqrt(T);
asserts.isequal(R.Properties.VariableUnits, {});
R = prod(T);
asserts.isequal(R.Properties.VariableUnits, {});
R = var(T);
asserts.isequal(R.Properties.VariableUnits, {});
%=============================================================================
R = sum(T);
asserts.isequal(R.Properties.VariableUnits, {'m', 'm'});
R = mean(T);
asserts.isequal(R.Properties.VariableUnits, {'m', 'm'});
R = std(T);
asserts.isequal(R.Properties.VariableUnits, {'m', 'm'});
R = sum(T, 2);
asserts.isequal(R.Properties.VariableUnits, {'m'});
%=============================================================================
V = T;
V.Properties.VariableUnits = {'m', 's'};
asserts.checkerror('sum(V, 2)', _('Table variable units must be compatible.'));
%=============================================================================
