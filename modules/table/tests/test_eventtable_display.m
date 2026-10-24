%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ET = eventtable(seconds([1; 2]));
R = evalc('ET');
assert_istrue(contains(R, ['2', char(215), '1 eventtable']));
assert_istrue(contains(R, '  Event Labels Variable: EventLabels'));
assert_istrue(contains(R, '  Event Lengths Variable: <instantaneous>'));
assert_istrue(contains(R, 'Time     EventLabels'));
assert_isfalse(contains(R, 'Event Ends Variable'));
%=============================================================================
% disp shows the contents only
R = evalc('disp(ET)');
assert_isfalse(contains(R, 'Event Labels Variable'));
assert_istrue(contains(R, 'Time     EventLabels'));
%=============================================================================
ET = eventtable(seconds([1; 2]), 'EventEnds', seconds([3; 4]));
R = evalc('ET');
assert_istrue(contains(R, '  Event Ends Variable: EventEnds'));
assert_isfalse(contains(R, 'Event Lengths Variable'));
%=============================================================================
ET = eventtable(seconds(zeros(0, 1)));
R = evalc('ET');
assert_istrue(contains(R, ['0', char(215), '0 empty eventtable']));
assert_istrue(contains(R, '  Event Labels Variable: <unset>'));
%=============================================================================
% a timetable has no event lines
TT = timetable(seconds([1; 2]), [3; 4]);
R = evalc('TT');
assert_isfalse(contains(R, 'Event'));
%=============================================================================
