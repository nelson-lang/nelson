%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mcpinfo'), 0);
asserts.isequal(nargout('mcpinfo'), 1);
%=============================================================================
info = mcpinfo();
asserts.istrue(isstruct(info));
asserts.isequal(info.name, 'nelson');
asserts.isequal(info.transport, 'stdio');
asserts.isequal(info.defaultDisplayMode, 'adv-cli');
asserts.isequal(info.workspaceRoot, nelsonroot());
asserts.isequal(info.maxOutputCharacters, 200000);
asserts.istrue(iscell(info.tools));
asserts.istrue(any(strcmp(info.tools, 'evaluate_nelson_code')));
asserts.istrue(any(strcmp(info.tools, 'create_nelson_plot')));
asserts.istrue(any(strcmp(info.tools, 'get_nelson_workspace')));
asserts.istrue(iscell(info.resources));
asserts.istrue(any(strcmp(info.resources, 'guidelines://nelson-coding')));
asserts.istrue(any(strcmp(info.resources, 'skills://nelson/catalog')));
asserts.istrue(any(strcmp(info.resources, 'skills://nelson-core/debugging')));
asserts.istrue(any(strcmp(info.resources, 'skills://nelson-core/testing')));
asserts.istrue(any(strcmp(info.resources, 'skills://nelson-core/packaging')));
asserts.istrue(any(strcmp(info.resources, 'skills://nelson-data/analysis-workflows')));
asserts.istrue(any(strcmp(info.resources, 'skills://nflow/create-block')));
asserts.istrue(any(strcmp(info.resources, 'skills://nflow/create-library')));
asserts.istrue(any(strcmp(info.resources, 'skills://nflow/create-model')));
asserts.istrue(iscell(info.prompts));
asserts.istrue(any(strcmp(info.prompts, 'nelson-code-review')));
asserts.isequal(info.allowExecution, true);
asserts.isequal(info.allowFormat, true);
%=============================================================================
