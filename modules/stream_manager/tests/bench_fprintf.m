%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
N = 5000;
fmt = 'fprintf benchmark %05d %.6f %s\n';
payload = 'payload';
expectedLine = sprintf(fmt, 1, 1 / 7, payload);
%=============================================================================
fprintf(stdout, 'bench_fprintf stdout: N=%d\n', N);
tic();
capturedOutput = evalc('for k = 1:N; fprintf(stdout, fmt, k, k / 7, payload); end');
elapsed = toc();
asserts.istrue(length(capturedOutput) >= N * length(expectedLine));
fprintf(stdout, 'stdout elapsed: %.6f s\n', elapsed);
%=============================================================================
plainLine = 'fprintf plain text fast path without escape sequences';
fprintf(stdout, 'bench_fprintf plain text fast path: N=%d\n', N);
tic();
plainOutput = evalc('for k = 1:N; fprintf(stdout, ''%s\n'', plainLine); end');
plainElapsed = toc();
asserts.isequal(length(plainOutput), N * (length(plainLine) + 1));
fprintf(stdout, 'plain text elapsed: %.6f s\n', plainElapsed);
%=============================================================================
clear capturedOutput;
clear plainOutput;
%=============================================================================
