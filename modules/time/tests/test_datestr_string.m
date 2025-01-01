%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
current_date=datevec(now());
%=============================================================================
R = datestr('05:32');
REF = sprintf('01-Jan-%d 05:32:00', current_date(1));
assert_isequal(R, REF);
%=============================================================================
R = datestr(["05:32","05:35"]);
REF1 = sprintf('01-Jan-%d 05:32:00', current_date(1));
REF2 = sprintf('01-Jan-%d 05:35:00', current_date(1));
REF = [REF1;REF2];
assert_isequal(R, REF);
%=============================================================================
R = datestr({'05:32', '05:35'});
REF = [REF1;REF2];
assert_isequal(R, REF);
%=============================================================================
R = datestr('05:32 PM','HH:MM');
REF = '17:32';
assert_isequal(R, REF);
%=============================================================================
formatOut = 'dd mmm yyyy';
R = datestr(datenum('16-04-55','dd-mm-yy',1900),formatOut);
REF = '16 Apr 1955';
assert_isequal(R, REF);
%=============================================================================
R = datestr(datenum({'09/16/2007';'05/14/1996';'11/29/2010'}, 'mm/dd/yyyy'));
REF = [    '16-Sep-2007'
'14-May-1996'
'29-Nov-2010'];
assert_isequal(R, REF);
%=============================================================================
cmd = "R = datestr(datenum('13/24/88','mm/dd/yy'));";
msg = _('Failed to convert text to date number.');
assert_checkerror(cmd, msg);
%=============================================================================
DateStringIn = '4/16/55';
formatOut = 1;
PivotYear = 1900;
R = datestr(DateStringIn,formatOut,PivotYear);
REF = '16-Apr-1955'; % Happy birthday! Jack ;) 
assert_isequal(R, REF);
%=============================================================================
PivotYear = 2000;
R = datestr(DateStringIn,formatOut,PivotYear);
REF = '16-Apr-2055';
%=============================================================================
R = datestr('05:32','HH:MM PM');
REF = ' 5:32 AM';
assert_isequal(R, REF);
%=============================================================================
