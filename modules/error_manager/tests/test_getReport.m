%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
exception = MException('Nelson:test:report', 'Report message.');
%=============================================================================
asserts.isequal(getReport(exception, 'basic'), 'Report message.');
asserts.isequal(exception.getReport('basic'), 'Report message.');
%=============================================================================
report = getReport(exception);
asserts.istrue(ischar(report));
asserts.istrue(contains(report, 'Report message.'));
%=============================================================================
