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
qml_file = [modulepath('qml_engine', 'tests'), '/test_qvariant_qt6.qml'];
qobj = qml_loadfile(qml_file);
%=============================================================================
asserts.isequal(class(qobj.active), 'logical');
asserts.isequal(class(qobj.children), 'QObject');
asserts.isequal(class(qobj.color), 'int32');
asserts.isequal(size(qobj.color), [1 4]);
asserts.isequal(class(qobj.contentItem), 'QObject');
asserts.isequal(class(qobj.height), 'int32');
asserts.isequal(class(qobj.maximumHeight), 'int32');
asserts.isequal(class(qobj.maximumWidth), 'int32');
asserts.isequal(class(qobj.minimumHeight), 'int32');
asserts.isequal(class(qobj.minimumWidth), 'int32');
asserts.isequal(class(qobj.objectName), 'char');
asserts.isequal(class(qobj.opacity), 'double');
asserts.isequal(class(qobj.title), 'char');
asserts.isequal(class(qobj.visible), 'logical');
asserts.isequal(class(qobj.width), 'int32');
asserts.isequal(class(qobj.x), 'int32');
asserts.isequal(class(qobj.y), 'int32');
%=============================================================================
asserts.checkerror('qobj.active=3', _('logical expected.'));
%=============================================================================
qobj.active = false;
asserts.isequal(qobj.active, false);
asserts.isequal(qobj.color, int32([255 255 255 255]));
asserts.isequal(qobj.height, int32(360));
asserts.isequal(qobj.maximumHeight, int32(16777215));
asserts.isequal(qobj.maximumWidth, int32(16777215));
asserts.isequal(qobj.minimumHeight, int32(0));
asserts.isequal(qobj.minimumWidth, int32(0));
asserts.isequal(qobj.objectName, 'MyWindow');
asserts.isequal(qobj.opacity, 1);
asserts.isequal(qobj.title, 'test_qvariant');
asserts.isequal(qobj.visible, false);
asserts.isequal(qobj.width, int32(360));
qobj.x = 780;
asserts.isequal(qobj.x, int32(780));
qobj.y = 335;
asserts.isequal(qobj.y, int32(335));
%=============================================================================
c = cell(2, 3);
for k = 1:6
  c{k} = k;
end
qobj.cell = c;
C = qobj.cell;
asserts.isequal(size(C), [1 2]);
REF = {{1, 3, 5}, {2, 4, 6}};
asserts.isequal(C, REF);
%=============================================================================
s.a = 1;
s.b = 'test';
s.c = c;
qobj.struct = s;
C = qobj.struct;
asserts.isequal(size(C), [1 1]);
clear REF
REF.a = 1;
REF.b = 'test';
REF.c = {{1, 3, 5}, {2, 4, 6}};
asserts.isequal(C, REF);
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
qobj.table = T;
S = qobj.table;
asserts.isequal(class(S), 'struct');
asserts.isequal(fieldnames(S.data), {'A'; 'B'});
if iscell(S.data.A)
  if isequal(size(S.data.A), [1 2])
    asserts.isequal(S.data.A, {1, 2});
    asserts.isequal(S.data.B, {3, 4});
  else
    asserts.isequal(S.data.A, {{1}, {2}});
    asserts.isequal(S.data.B, {{3}, {4}});
  end
else
  asserts.isequal(S.data.A, [1; 2]);
  asserts.isequal(S.data.B, [3; 4]);
end
asserts.isequal(S.Properties.VariableNames, {'A', 'B'});
asserts.isequal(S.Properties.RowNames, {'r1', 'r2'});
%=============================================================================
delete(qobj)
%=============================================================================
