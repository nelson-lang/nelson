%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
vbScript = actxserver('MSScriptControl.ScriptControl');
set(vbScript, 'Language', 'VBScript');
invoke(vbScript,'Eval','MsgBox(""Hello world"")');
r = invoke(vbScript,'Eval','68 + 1')
class(r)
delete(vbScript);
%=============================================================================
