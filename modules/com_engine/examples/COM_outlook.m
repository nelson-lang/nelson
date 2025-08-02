%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
pOutlook = actxserver('Outlook.Application')
pItem = get(pOutlook, 'CreateItem', 0)
set(pItem, 'Subject','[TEST COM INTERFACE] hello')
set(pItem, 'To', 'your.email@gmail.com')
set(pItem, 'Body','Test to send email with COM interface [OK]')
invoke(pItem, 'Display')
invoke(pItem, 'Send')
delete(pItem)
delete(pOutlook)
clear pOutlook pItem
%=============================================================================
