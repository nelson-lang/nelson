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
if semver(qt_version(), '>=6.0')
  qml_file = [modulepath('qml_engine', 'tests'), '/test_qvariant_qt6.qml'];
else
  qml_file = [modulepath('qml_engine', 'tests'), '/test_qvariant_qt5.qml'];
end 
qobj = qml_loadfile(qml_file);
%=============================================================================
assert_isequal(class(qobj.active), 'logical');
assert_isequal(class(qobj.children), 'QObject');
assert_isequal(class(qobj.color), 'int32');
assert_isequal(size(qobj.color), [1 4]);
assert_isequal(class(qobj.contentItem), 'QObject');
assert_isequal(class(qobj.height), 'int32');
assert_isequal(class(qobj.maximumHeight),'int32');
assert_isequal(class(qobj.maximumWidth),'int32');
assert_isequal(class(qobj.minimumHeight),'int32');
assert_isequal(class(qobj.minimumWidth),'int32');
assert_isequal(class(qobj.objectName),'char');
assert_isequal(class(qobj.opacity),'double');
assert_isequal(class(qobj.title),'char');
assert_isequal(class(qobj.visible),'logical');
assert_isequal(class(qobj.width),'int32');
assert_isequal(class(qobj.x),'int32');
assert_isequal(class(qobj.y),'int32');
%=============================================================================
assert_checkerror('qobj.active=3', _('logical expected.'));
%=============================================================================
qobj.active = false;
assert_isequal(qobj.active, false);
assert_isequal(qobj.color, int32([255 255 255 255]));
assert_isequal(qobj.height, int32(360));
assert_isequal(qobj.maximumHeight, int32(16777215));
assert_isequal(qobj.maximumWidth, int32(16777215));
assert_isequal(qobj.minimumHeight, int32(0));
assert_isequal(qobj.minimumWidth, int32(0));
assert_isequal(qobj.objectName, 'MyWindow');
assert_isequal(qobj.opacity, 1);
assert_isequal(qobj.title, 'test_qvariant');
assert_isequal(qobj.visible, false);
assert_isequal(qobj.width, int32(360));
qobj.x = 780;
assert_isequal(qobj.x, int32(780));
qobj.y = 335;
assert_isequal(qobj.y, int32(335));
%=============================================================================
c = cell(2, 3);
for k = 1:6
  c{k} = k;
end
qobj.cell = c;
C = qobj.cell;
assert_isequal(size(C), [1 2]);
REF = {{1, 3, 5}, {2, 4, 6}};
assert_isequal(C, REF);
%=============================================================================
s.a = 1;
s.b = 'test';
s.c = c;
qobj.struct = s;
C = qobj.struct;
assert_isequal(size(C), [1 1]);
clear REF
REF.a = 1;
REF.b = 'test';
REF.c = {{1, 3, 5}, {2, 4, 6}};
assert_isequal(C, REF);
%=============================================================================
delete(qobj)
%=============================================================================
