%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the built-in catalog ships nflow skills, discovered by nelson.ai.skills
allSkills = nelson.ai.skills();
names = lower(string({allSkills.name}));
assert_istrue(any(contains(names, "nflow")));
%=============================================================================
% the assistant can locate nflow block-diagram builtins through its tools,
% so the agent can build and run models with evaluate_nelson_code
r = nelson.ai.mcp.call('find_nelson_symbol', struct('symbol', 'new_system'));
assert_istrue(r.success);
located = [r.output, r.data];
assert_istrue(contains(located, 'new_system'));
%=============================================================================
% a matching nflow skill is selected by trigger wording
modelSkills = nelson.ai.skills('build an nflow block diagram model');
assert_istrue(~isempty(modelSkills));
%=============================================================================
