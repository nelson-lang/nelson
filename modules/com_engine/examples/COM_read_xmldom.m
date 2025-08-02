%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
xmlFileName = [modulepath('com_engine'),  '/examples/TheSimpsons.xml'];
xmlDoc = actxserver( 'Microsoft.XMLDOM');

% load xml file in memory
set(xmlDoc, 'async', false);
invoke(xmlDoc, 'load', xmlFileName);

% plain text
get(xmlDoc,'text')

% Get Name of Characters
oElement = get(xmlDoc,'documentElement');
x = invoke(oElement, 'getElementsByTagName' ,'name');
Lx = get(x,'length');
for i = 0:double(Lx) - 1
  pItem = get(x, 'item', i);
  disp(get(pItem,'text'));
end
delete(oElement);

% Get Firstname of Characters
oElement = get(xmlDoc,'documentElement');
x = invoke(oElement, 'getElementsByTagName', 'firstname');
Lx = get(x,'length');
for i = 0:double(Lx) - 1
  pItem = get(x, 'item', i);
  disp(get(pItem, 'text'));
end
delete(oElement);
delete(xmlDoc);
%=============================================================================
