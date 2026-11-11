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
asserts.isequal(ME.stack(1).line, 11);
asserts.isfalse(any(strcmp({ME.stack.name}, 'istrue')));
%=============================================================================
% equality and approximation failures are reported at the caller line
d = [tempname(), '_assert_stack'];
mkdir(d);
calls = {'asserts.isequal(a, b)', 'assert_isequal(a, b)', 'asserts.isapprox(a, b, 1e-6)', ...
  'assert_isapprox(a, b, 1e-6)'};
for k = 1:numel(calls)
  name = sprintf('assert_stack_probe_%d', k);
  lines = {['function ', name, '()'], '  a = 1;', '  b = 2;', ['  ', calls{k}, ';'], 'end'};
  filewrite([d, '/', name, '.m'], strjoin(lines, newline));
end
addpath(d);
for k = 1:numel(calls)
  name = sprintf('assert_stack_probe_%d', k);
  raised = false;
  try
    feval(name);
  catch ME
    raised = true;
  end
  asserts.istrue(raised);
  asserts.isequal(ME.identifier, 'Nelson:asserts:assertionFailed');
  asserts.isequal(ME.stack(1).name, name);
  asserts.isequal(ME.stack(1).line, 4);
end
rmpath(d);
rmdir(d, 's');
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

