%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Display of large duration and calendarDuration arrays is linear: the text is
% rendered in one vectorized pass (durationRenderArray / calendarDuration
% string) and printed by the shared matrixText without per-row buffer growth.
% A quadratic path would not finish these sizes in reasonable time.
n = 16000;
%=============================================================================
% duration, default seconds format (column vector).
du = seconds(0:n - 1)';
tic();
txt = evalc('disp(du)');
toc();
asserts.istrue(contains(txt, '0 sec'));
asserts.istrue(contains(txt, sprintf('%d sec', n - 1)));
%=============================================================================
% duration, clock format.
duc = duration.fromSeconds((0:n - 1)' + 0.5, 'hh:mm:ss');
tic();
txt = evalc('disp(duc)');
toc();
asserts.istrue(contains(txt, '00:00:00'));
%=============================================================================
% calendarDuration, calendar months (column vector).
cdu = calmonths(0:n - 1)';
tic();
txt = evalc('disp(cdu)');
toc();
asserts.istrue(contains(txt, '0d'));
%=============================================================================
% calendarDuration with every component present (the widest element text).
cdf = calendarDuration(0, (1:n)', 1, 0, 0, 3661.25);
tic();
txt = evalc('disp(cdf)');
toc();
asserts.istrue(contains(txt, '1mo 1d 1h 1m 1.25s'));
%=============================================================================
