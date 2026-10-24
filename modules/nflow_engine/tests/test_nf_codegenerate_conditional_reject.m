%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% What is left of the conditional-execution reject, now that most of it has
% been answered. Every control kind lowers - enable, action, trigger,
% enableTrigger, reset - at any nesting depth, from wherever the spec keeps its
% control port, and under the fixed-step RK4 seam as well; so does the
% if / switchCase / merge action-selector family. See
% test_nf_codegenerate_cond_enable.m, _cond_enable_ode4.m, _cond_reset_ode4.m,
% _cond_nested.m, _cond_enabletrigger.m and _cond_selector.m.
%
% What is left is refused for a stated - and measured - reason rather than for
% want of work: a body that runs SEVERAL times within one step, carrying state
% from one pass to the next, while the flattener inlines a body once. That is a
% for-iterator, and a function-call generator firing more than once per step
% (one per step lowers, see test_nf_codegenerate_cond_functioncall.m). Plus a
% reset gate under the VARIABLE-step solver, which can reject a step and retry
% it - and so replay the re-seed.
%=============================================================================
dest = tempname();

function write_nflow(fname, d)
  fid = fopen(fname, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
end

% --- Function-call generator firing THREE times per step: rejected. Those
% calls are a loop within the step; one call per step lowers. ---------------
d1 = struct();
d1.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sel', 'type', 'functionCallGenerator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('NumberOfIterations', 3))};
d1.connections = {struct('from', 'clk', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0)};
f1 = [tempname(), '.nflow'];
write_nflow(f1, d1);
msg1 = '';
try
  nflow_codegenerate(f1, dest, true);
catch e1
  msg1 = e1.message;
end
asserts.istrue(~isempty(strfind(lower(msg1), 'conditional')));

%=============================================================================
