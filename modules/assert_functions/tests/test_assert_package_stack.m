%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/assert_functions/tests']);
%=============================================================================
raised = false;
try
  asserts_stack_probe();
catch ME
  raised = true;
end
asserts.istrue(raised);
asserts.istrue(numel(ME.stack) >= 1);
asserts.isequal(ME.stack(1).name, 'asserts_stack_probe');
asserts.isequal(ME.stack(1).line, 2);
asserts.isfalse(any(strcmp({ME.stack.name}, 'istrue')));
%=============================================================================
raised = false;
try
  asserts.satisfies(1, 'asserts_satisfies_error_probe');
catch ME
  raised = true;
end
asserts.istrue(raised);
asserts.isequal(ME.identifier, 'Nelson:assertsStackProbe:userError');
asserts.istrue(any(strcmp({ME.stack.name}, 'asserts_satisfies_error_probe')));
%=============================================================================

