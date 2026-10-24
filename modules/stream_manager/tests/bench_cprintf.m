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
fmt = 'cprintf benchmark %05d %.6f %s\n';
payload = 'payload';
expectedLine = sprintf(fmt, 1, 1 / 7, payload);
%=============================================================================
fprintf(stdout, 'bench_cprintf text style: N=%d\n', N);
tic();
textOutput = evalc('for k = 1:N; cprintf(''text'', fmt, k, k / 7, payload); end');
textElapsed = toc();
asserts.istrue(length(textOutput) >= N * length(expectedLine));
fprintf(stdout, 'text style elapsed: %.6f s\n', textElapsed);
%=============================================================================
fprintf(stdout, 'bench_cprintf styled: N=%d\n', N);
tic();
styledOutput = evalc('for k = 1:N; cprintf(''*cyan'', fmt, k, k / 7, payload); end');
styledElapsed = toc();
asserts.isequal(length(styledOutput), length(textOutput));
fprintf(stdout, 'styled elapsed: %.6f s\n', styledElapsed);
%=============================================================================
clear textOutput styledOutput;
%=============================================================================
