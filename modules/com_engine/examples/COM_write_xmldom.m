%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% write a xml file
xmlDoc = actxserver('Microsoft.XMLDOM')

% create root element
oRoot = invoke(xmlDoc, 'createElement', 'Root');
invoke(xmlDoc, 'appendChild', oRoot);

% add element
oElement = invoke(xmlDoc, 'selectSingleNode', 'Root');
% create a child
oElement = invoke(xmlDoc, 'createElement' , 'element');

pdocumentElement = xmlDoc.documentElement;
% add child to root element
invoke(pdocumentElement, 'appendChild', oElement);

oName = invoke(xmlDoc,'createElement','name');
oName.Text = 'CORNET';
invoke(oElement, 'appendChild', oName);

oFirstname = invoke(xmlDoc,'createElement','firstname');
set(oFirstname,'Text', 'Allan');
invoke(oElement, 'appendChild', oFirstname);


% Save file
invoke(xmlDoc, 'Save', [tempdir(), 'write.xml']);
disp(fileread([tempdir(), 'write.xml']))

delete(xmlDoc);
clear xmlDoc
%=============================================================================

