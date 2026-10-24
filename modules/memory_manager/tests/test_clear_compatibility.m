%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear variables
%=============================================================================
MonValue = 1;
TueValue = 2;
KeepValue = 3;
clear('-regexp', '^Mon', '^Tue');
asserts.isfalse(isvar('MonValue'));
asserts.isfalse(isvar('TueValue'));
asserts.istrue(isvar('KeepValue'));
clear KeepValue
%=============================================================================
StringNamedValue = 4;
clear("StringNamedValue");
asserts.isfalse(isvar('StringNamedValue'));
%=============================================================================
global clearGlobalOne clearGlobalTwo
clearGlobalOne = 1;
clearGlobalTwo = 2;
clear global clearGlobalOne clearGlobalTwo
asserts.isfalse(isglobal('clearGlobalOne'));
asserts.isfalse(isglobal('clearGlobalTwo'));
%=============================================================================
RegexFunctionValue = 5;
clear("-regexp", "^RegexFunction");
asserts.isfalse(isvar('RegexFunctionValue'));
%=============================================================================
asserts.checkerror('clear(''-regexp'', ''['')', _('Invalid regular expression.'));
%=============================================================================
ClearvarsKeepA = 1;
ClearvarsDropB = 2;
ClearvarsKeepC = 3;
clearvars('-except', 'ClearvarsKeepA', 'ClearvarsKeepC');
asserts.istrue(isvar('ClearvarsKeepA'));
asserts.isfalse(isvar('ClearvarsDropB'));
asserts.istrue(isvar('ClearvarsKeepC'));
clearvars ClearvarsKeepA ClearvarsKeepC
%=============================================================================
ClearvarsNamedA = 1;
ClearvarsNamedB = 2;
ClearvarsNamedC = 3;
clearvars('ClearvarsNamedA', 'ClearvarsNamedC', '-except', 'ClearvarsNamedC');
asserts.isfalse(isvar('ClearvarsNamedA'));
asserts.istrue(isvar('ClearvarsNamedB'));
asserts.istrue(isvar('ClearvarsNamedC'));
clearvars ClearvarsNamedB ClearvarsNamedC
%=============================================================================
ClearvarsWildA = 1;
ClearvarsWildB = 2;
ClearvarsOther = 3;
clearvars('ClearvarsWild*', '-except', 'ClearvarsWildB');
asserts.isfalse(isvar('ClearvarsWildA'));
asserts.istrue(isvar('ClearvarsWildB'));
asserts.istrue(isvar('ClearvarsOther'));
clearvars ClearvarsWildB ClearvarsOther
%=============================================================================
ClearvarsRegexA = 1;
ClearvarsRegexB = 2;
ClearvarsRegexOther = 3;
clearvars('-regexp', '^ClearvarsRegex[AB]$', '-except', 'ClearvarsRegexB');
asserts.isfalse(isvar('ClearvarsRegexA'));
asserts.istrue(isvar('ClearvarsRegexB'));
asserts.istrue(isvar('ClearvarsRegexOther'));
clearvars ClearvarsRegexB ClearvarsRegexOther
%=============================================================================
ClearvarsKeepRegexpA = 1;
ClearvarsKeepRegexpB = 2;
ClearvarsKeepRegexpC = 3;
clearvars('ClearvarsKeepRegexp*', '-except', '-regexp', 'B$');
asserts.isfalse(isvar('ClearvarsKeepRegexpA'));
asserts.istrue(isvar('ClearvarsKeepRegexpB'));
asserts.isfalse(isvar('ClearvarsKeepRegexpC'));
clearvars ClearvarsKeepRegexpB
%=============================================================================
ClearvarsRegexpAllA = 1;
ClearvarsRegexpAllB = 2;
clearvars('-regexp');
asserts.isfalse(isvar('ClearvarsRegexpAllA'));
asserts.isfalse(isvar('ClearvarsRegexpAllB'));
%=============================================================================
global ClearvarsGlobalKeep ClearvarsGlobalDrop
ClearvarsGlobalKeep = 1;
ClearvarsGlobalDrop = 2;
clearvars('-global', '-except', 'ClearvarsGlobalKeep');
asserts.istrue(isglobal('ClearvarsGlobalKeep'));
asserts.isfalse(isglobal('ClearvarsGlobalDrop'));
clear global ClearvarsGlobalKeep ClearvarsGlobalDrop
%=============================================================================
global ClearvarsGlobalLocal ClearvarsGlobalOther
ClearvarsGlobalLocal = 10;
ClearvarsGlobalOther = 20;
clearvars ClearvarsGlobalLocal
asserts.isfalse(isvar('ClearvarsGlobalLocal'));
global ClearvarsGlobalLocal
asserts.isequal(ClearvarsGlobalLocal, 10);
clear global ClearvarsGlobalLocal ClearvarsGlobalOther
%=============================================================================
ClearvarsCellA = 1;
ClearvarsCellB = 2;
ClearvarsCellC = 3;
ClearvarsInitial = {'ClearvarsCellA', 'ClearvarsCellC'};
clearvars('-except', ClearvarsInitial{:});
asserts.istrue(isvar('ClearvarsCellA'));
asserts.isfalse(isvar('ClearvarsCellB'));
asserts.istrue(isvar('ClearvarsCellC'));
asserts.isfalse(isvar('ClearvarsInitial'));
clearvars ClearvarsCellA ClearvarsCellC
%=============================================================================
asserts.checkerror('clearvars(''-regexp'', ''['')', _('Invalid regular expression.'));
asserts.checkerror('clearvars(''ClearvarsInvalid'', ''-except'', ''-regexp'', ''['')', _('Invalid regular expression.'));
%=============================================================================
