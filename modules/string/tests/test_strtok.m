%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[tok, rem] = strtok('  abc def');
asserts.isequal(tok, 'abc');
asserts.isequal(rem, ' def');
str = ["all in good time"; "my dog has fleas"; "leave no stone unturned"];
[tok, rem] = strtok(str);
asserts.isequal(tok, ["all"; "my"; "leave"]);
asserts.isequal(rem, [" in good time"; " dog has fleas"; " no stone unturned"]);
[tok, rest] = strtok("中文;测试", ";");
asserts.isequal(tok, "中文");
asserts.isequal(rest, ";测试");
%=============================================================================
