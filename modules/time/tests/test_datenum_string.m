%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = datenum('04-Aug-1973 12:01:18');
REF = 720840.500902777770534;
assert_isequal(d, REF);
%=============================================================================
dv = datevec(datenum('10-Mar-2010 16:48:17'));
REF = [2010           3          10          16          48          17];
assert_isequal(fix(dv), REF);
%=============================================================================
dv = datevec(datenum('12-Mar-2010'));
REF = [2010           3          12           0           0           0];
assert_isequal(fix(dv), REF);
%=============================================================================
dv = datevec(datenum('03/10/2010'));
REF = [2010           3          10           0           0           0];
assert_isequal(dv, REF);
%=============================================================================
dv = datevec(datenum('03/10/00'));
REF = [2000           3          10           0           0           0];
assert_isequal(dv, REF);
%=============================================================================
dv = datevec(datenum('03/10'));
REF = [2024           3          10           0           0           0];
assert_isequal(dv(2:end), REF(2:end));
%=============================================================================
dv = datevec(datenum('2010-03-10 16:48:17'));
REF = [2010           3          10          16          48          17];
assert_isequal(fix(dv), REF);
%=============================================================================
dv = datevec(datenum('2010-03-10'));
REF = [2010           3          10           0           0           0];
assert_isequal(dv, REF);
%=============================================================================
dv = datevec(datenum('2000/03/10'));
REF = [2000           3          10           0           0           0];
assert_isequal(fix(dv), REF);
%=============================================================================
dv = datevec(datenum('16:48:17'));
r = datevec(now());
REF = [r(1), 1, 1, 16, 48, 17];
assert_isequal(fix(dv), REF);
%=============================================================================
dv = datevec(datenum('03:48:17 PM'));
REF = [r(1), 1, 1, 15, 48, 17];
assert_isequal(dv, REF);
%=============================================================================
dv = datevec(datenum('03:48:17 AM'));
REF = [r(1), 1, 1, 3, 48, 17];
assert_isequal(dv, REF);
%=============================================================================
dn = datenum('16:48');
REF = datenum([r(1), 1, 1, 16, 48, 0]);
assert_isequal(dn, REF);
%=============================================================================
dn = datenum('03:35 PM');
REF = datenum([r(1), 1, 1, 15, 35, 0]);
assert_isequal(dn, REF);
%=============================================================================
dn = datenum('03:35 AM');
REF = datenum([r(1), 1, 1, 3, 35, 0]);
assert_isequal(dn, REF);
%=============================================================================
R = datenum(["2000/03/10";"2000/03/11"]);
REF = [730555; 730556];
assert_isequal(R, REF);
%=============================================================================
R = datenum({'2000/03/10';'2000/03/11'});
REF = [730555; 730556];
assert_isequal(R, REF);
%=============================================================================
R = datenum('19-May-2001', 'dd-mmm-yyyy');
REF = 730990;
assert_isequal(R, REF);
%=============================================================================
% REF date 2009/01/31 13:56:23
%=============================================================================
R = datevec(datenum('31-Jan-2009 13:56:23', 'dd-mmm-yyyy HH:MM:SS'));
REF = [2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31-Jan-2009 13:56:23', 'dd-mmm-yyyy HH:MM:SS', 100));
REF = [2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31-Jan-2009  13:56:23', 'dd-mmm-yyyy'));
REF = [ 2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31-Jan-2009', 'dd-mmm-yyyy'));
REF = [ 2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31-Jan-2009', 'dd-mmm-yyyy', 100));
REF = [2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/09', 'mm/dd/yy'));
REF = [2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/09', 'mm/dd/yy', 100));
REF = [109     1    31     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31','mm/dd'));
REF = [ 2024           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31','mm/dd', 100));
REF = [2024           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/2009', 'mm/dd/yyyy'));
REF = [2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/2009', 'mm/dd/yyyy', 100));
REF = [2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31/01/09', 'dd/mm/yy'));
REF = [ 2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31/01/09', 'dd/mm/yy',100));
REF = [ 109     1    31     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/09', 'mm/dd/yy'));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/09', 'mm/dd/yy', 100));
REF = [109     1    31     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan09', 'mmmyy'));
REF = [2009           1           1           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan09', 'mmmyy', 100));
REF = [ 109     1     1     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009/01/31', 'yyyy/mm/dd'));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009/01/31', 'yyyy/mm/dd', 100));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31', 'yyyy-mm-dd'));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31', 'yyyy-mm-dd', 100));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31 13:56:23', 'yyyy-mm-dd HH:MM:SS'));
REF = [  2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31 13:56:23', 'yyyy-mm-dd HH:MM:SS', 100));
REF = [   2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('JAN-31-2009 13:56:23', 'mmm-dd-yyyy HH:MM:SS'));
REF = [   2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('JAN-31-2009 13:56:23', 'mmm-dd-yyyy HH:MM:SS', 100));
REF = [   2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan-31-2009', 'mmm-dd-yyyy'));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan-31-2009', 'mmm-dd-yyyy', 100));
REF = [  2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31 Jan 2009 13:56:23', 'dd mmm yyyy HH:MM:SS'));
REF = [  2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31 Jan 2009 13:56:23', 'dd mmm yyyy HH:MM:SS', 100));
REF = [  2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31 Jan 2009', 'dd mmm yyyy'));
REF = [ 2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31 Jan 2009', 'dd mmm yyyy', 100));
REF = [ 2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan 31 2009 13:56:23', 'mmm dd yyyy HH:MM:SS'));
REF = [ 2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan 31 2009 13:56:23', 'mmm dd yyyy HH:MM:SS', 100));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan 31 2009', 'mmm dd yyyy'));
REF = [        2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan 31 2009', 'mmm dd yyyy', 100));
REF = [        2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31.Jan.2009 13:56:23', 'dd.mmm.yyyy HH:MM:SS'));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31.Jan.2009 13:56:23', 'dd.mmm.yyyy HH:MM:SS', 100));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31.Jan.2009', 'dd.mmm.yyyy'));
REF = [        2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('31.Jan.2009', 'dd.mmm.yyyy', 100));
REF = [        2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan.31.2009 13:56:23', 'mmm.dd.yyyy HH:MM:SS'));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan.31.2009 13:56:23', 'mmm.dd.yyyy HH:MM:SS', 100));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan.31.2009', 'mmm.dd.yyyy'));
REF = [        2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan.31.2009', 'mmm.dd.yyyy', 100));
REF = [        2009           1          31           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/2009 13:56', 'mm/dd/yyyy HH:MM'));
REF = [        2009           1          31          13          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/2009 13:56', 'mm/dd/yyyy HH:MM', 100));
REF = [        2009           1          31          13          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('01/31/2009', 'yyyy'));
REF = [     1     1     1     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009', 'yyyy'));
REF = [        2009           1           1           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009', 'yyyy', 100));
REF = [        2009           1           1           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01', 'yyyy-mm'));
REF = [        2009           1           1           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01', 'yyyy-mm', 100));
REF = [        2009           1           1           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31T13:56:23Z', 'yyyy-mm-ddTHH:MM:SSZ'));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31T13:56:23Z', 'yyyy-mm-ddTHH:MM:SSZ',100));
REF = [        2009           1          31          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('2009-01-31T13:56:23.145Z', 'yyyy-mm-ddTHH:MM:SS.FFFZ'));
REF = [2.009000000000000   0.001000000000000   0.031000000000000   0.013000000000000   0.056000000000000   0.0231] * 1e3;
assert_isapprox(R, REF, 1-3);
%=============================================================================
R = datevec(datenum('2009-01-31T13:56:23.145Z', 'yyyy-mm-ddTHH:MM:SS.FFFZ', 100));
REF = [   2.009000000000000   0.001000000000000   0.031000000000000   0.013000000000000   0.056000000000000   0.02314] * 1e3;
assert_isapprox(R, REF, 1-3);
%=============================================================================
R = datevec(datenum('13:56:23', 'HH:MM:SS'));
REF = [        2024           1           1          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('13:56:23', 'HH:MM:SS', 100));
REF = [        2024           1           1          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('13:56', 'HH:MM'));
REF = [        2024           1           1          13          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('13:56', 'HH:MM', 100));
REF = [        2024           1           1          13          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum("13:56:23 PM", "HH:MM:SS PM"));
REF = [2024           1           2           1          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum("13:56:23 PM", "HH:MM:SS PM", 100));
REF = [2024           1           2           1          56          23];
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('datenum("13:56:23", "HH:MM:SS PM")', _('Failed to convert text to date number.'));
%=============================================================================
cmd = "R = datevec(datenum('13:56', 'HH:MM PM'))";
msg = _('Failed to convert text to date number.');
assert_checkerror(cmd, msg);
%=============================================================================
R = datevec(datenum('13:56 AM', 'HH:MM AM'));
REF = [        2024           1           1          13          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('13:56 PM', 'HH:MM PM'));
REF = [        2024           1           2           1          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('13:56 PM', 'HH:MM PM', 100));
REF = [        2024           1           2           1          56           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('jan.012009', 'mmm.ddyyyy'));
REF = [ 2009           1           1           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('jan.012009', 'mmm.ddyyyy', 100));
REF = [2009     1     1     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datenum ("5-19, 2001", "mm-dd, yyyy");
REF = 730990;
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('25122015','ddmmyyyy'));
REF = [2015          12          25           0           0           0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Jan.012009 13:56:23', 'mmm.ddyyyy HH:MM:SS'));
REF = [  2009           1           1          13          56          23];
assert_isequal(R, REF);
%=============================================================================
R = datevec(datenum('Monday, Mar 20, 2014', 'dddd, mmm dd, yyyy'));
REF = [ 2014           3          20           0           0           0];
assert_isequal(R, REF);
%=============================================================================