%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
% A requested output that the body never assigns is an error.
msg = _("Output argument 'b' is not assigned during call to 'output_not_assigned_fixture'.");
asserts.checkerror('[p, q] = output_not_assigned_fixture(5)', msg);
%=============================================================================
% Not requesting the unassigned output is fine.
R = output_not_assigned_fixture(5);
asserts.isequal(R, 10);
%=============================================================================
% Calling at statement level (nargout == 0) does not require the output.
output_not_assigned_fixture(5);
%=============================================================================
% When every requested output is assigned there is no error.
[p, q] = output_not_assigned_fixture(200);
asserts.isequal(p, 400);
asserts.isequal(q, 1);
%=============================================================================
