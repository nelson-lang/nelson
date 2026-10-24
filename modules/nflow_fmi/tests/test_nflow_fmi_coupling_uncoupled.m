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
% Opt-in iterative co-simulation coupling must remain correct and safe for a
% single uncoupled FMU-CS block: the iterated run must reproduce the default
% single-pass run exactly, because the fixed point of one FMU is the FMU itself.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
root = modulepath('nflow_fmi', 'root');
d = jsondecode(fileread([root, '/examples/bouncingball_fmu.nflow']));
d.duration = str2double(d.runtime);
d.baseDir = [root, '/examples'];
d.solver = 'discrete';
%=============================================================================
% Default single pass.
r1 = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r1.status, 'done');
assert_istrue(numel(r1.scopes) >= 1);
%=============================================================================
% Opt-in iterative coupling: same diagram, converged rollback iteration.
d.iterativeCoupling = true;
d.couplingMaxIter = 20;
d.couplingTol = 1e-12;
r2 = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r2.status, 'done');
%=============================================================================
% The uncoupled FMU must land on exactly the same trajectory.
a = r1.scopes(1).series(:);
b = r2.scopes(1).series(:);
assert_isequal(numel(a), numel(b));
assert_istrue(max(abs(a - b)) < 1e-12);
%=============================================================================
