%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('cprintf'), -1);
asserts.isequal(nargout('cprintf'), 1);
%=============================================================================
R = evalc('count = cprintf(''plain text'');');
REF = 'plain text';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('count = cprintf(''red'');');
REF = 'red';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('count = cprintf('''', ''empty style'');');
REF = 'empty style';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('count = cprintf(''text'', ''text style'');');
REF = 'text style';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('count = cprintf(''number %d'', 42);');
REF = 'number 42';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('count = cprintf("string value");');
REF = 'string value';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('count = cprintf("red", "string style");');
REF = 'string style';
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('cprintf(''keywords'', ''keyword style'');');
asserts.isequal(R, 'keyword style');
%=============================================================================
R = evalc('cprintf(''cy'', ''cyan prefix'');');
asserts.isequal(R, 'cyan prefix');
%=============================================================================
R = evalc('cprintf(''lightgreen'', ''light variant'');');
asserts.isequal(R, 'light variant');
%=============================================================================
R = evalc('cprintf(''dark-blue'', ''dark variant'');');
asserts.isequal(R, 'dark variant');
%=============================================================================
R = evalc('cprintf([1 0.5 0], ''rgb normalized'');');
asserts.isequal(R, 'rgb normalized');
%=============================================================================
R = evalc('cprintf([0 128 255], ''rgb byte'');');
asserts.isequal(R, 'rgb byte');
%=============================================================================
R = evalc('cprintf(-[1 0 1], ''negative rgb underline'');');
asserts.isequal(R, 'negative rgb underline');
%=============================================================================
R = evalc('cprintf(''#0af'', ''short hex'');');
asserts.isequal(R, 'short hex');
%=============================================================================
R = evalc('cprintf(''#00aaff'', ''long hex'');');
asserts.isequal(R, 'long hex');
%=============================================================================
R = evalc('cprintf(''*red'', ''bold text'');');
asserts.isequal(R, 'bold text');
%=============================================================================
R = evalc('cprintf(''_green'', ''underlined text'');');
asserts.isequal(R, 'underlined text');
%=============================================================================
R = evalc('cprintf(''*_dark-magenta'', ''bold underline text'');');
asserts.isequal(R, 'bold underline text');
%=============================================================================
msg = ['accent ', char(233)];
R = evalc('count = cprintf(''comments'', ''line 1\n%s\n'', msg);');
REF = ['line 1', char(10), msg, char(10)];
asserts.isequal(R, REF);
asserts.isequal(count, length(REF));
%=============================================================================
R = evalc('cprintf(''errors'', ''stdout only'');');
asserts.isequal(R, 'stdout only');
%=============================================================================
R = evalc('count = cprintf();');
asserts.istrue(contains(R, 'cprintf demo'));
asserts.isequal(count, length(R));
%=============================================================================
asserts.checkerror('cprintf(''g'', ''bad'')', _('Ambiguous style name.'));
asserts.checkerror('cprintf(''g'', ''bad'')', _('Ambiguous style name.'), 'Nelson:cprintf:ambiguousStyleName');
asserts.checkerror('cprintf(''#00zz00'', ''bad'')', _('Valid style expected.'));
asserts.checkerror('cprintf(''#00zz00'', ''bad'')', _('Valid style expected.'), 'Nelson:cprintf:invalidStyle');
asserts.checkerror('cprintf(''unknownstyle'', ''bad'')', _('Valid style expected.'));
asserts.checkerror('cprintf([1 2], ''bad'')', _('Valid style expected.'));
asserts.checkerror('[a, b] = cprintf(''bad'')', _('Too many output arguments.'));
%=============================================================================
