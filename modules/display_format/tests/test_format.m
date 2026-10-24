%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
format('default');
%=============================================================================
f = format();
asserts.isequal(class(f), 'nelson.display.DisplayFormatOptions')
asserts.isequal(f.NumericFormat, "short")
asserts.isequal(f.LineSpacing, "loose")
asserts.isequal(f.TruncateMatrices, "on")
%=============================================================================
format('short');
f = format();
asserts.isequal(f.NumericFormat, "short")
asserts.isequal(f.LineSpacing, "loose")
asserts.isequal(f.TruncateMatrices, "on")
%=============================================================================
format('long');
f = format();
asserts.isequal(f.NumericFormat, "long")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('shortE');
f = format();
asserts.isequal(f.NumericFormat, "shortE")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('shortG');
f = format();
asserts.isequal(f.NumericFormat, "shortG")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('short', 'e');
f = format();
asserts.isequal(f.NumericFormat, "shortE")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('longE');
f = format();
asserts.isequal(f.NumericFormat, "longE")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('long', 'e');
f = format();
asserts.isequal(f.NumericFormat, "longE")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('longG');
f = format();
asserts.isequal(f.NumericFormat, "longG")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('long', 'g');
f = format();
asserts.isequal(f.NumericFormat, "longG")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('shortEng');
f = format();
asserts.isequal(f.NumericFormat, "shortEng")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('short', 'Eng');
f = format();
asserts.isequal(f.NumericFormat, "shortEng")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('longEng');
f = format();
asserts.isequal(f.NumericFormat, "longEng")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('long', 'Eng');
f = format();
asserts.isequal(f.NumericFormat, "longEng")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('+');
f = format();
asserts.isequal(f.NumericFormat, "+")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('bank');
f = format();
asserts.isequal(f.NumericFormat, "bank")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('hex');
f = format();
asserts.isequal(f.NumericFormat, "hex")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('rational');
f = format();
asserts.isequal(f.NumericFormat, "rational")
asserts.isequal(f.LineSpacing, "loose")
%=============================================================================
format('compact');
f = format();
asserts.isequal(f.NumericFormat, "rational")
asserts.isequal(f.LineSpacing, "compact")
%=============================================================================
format('loose');
f = format();
asserts.isequal(f.NumericFormat, "rational")
asserts.isequal(f.LineSpacing, "loose")
asserts.isequal(f.TruncateMatrices, "on")
%=============================================================================
format('truncateMatrices', 'off');
f = format();
asserts.isequal(f.TruncateMatrices, "off")
format('truncateMatrices', 'on');
f = format();
asserts.isequal(f.TruncateMatrices, "on")
%=============================================================================
oldFormat = format();
format('short');
format('compact');
format('truncateMatrices', 'off');
fmtDefault = nelson.display.DisplayFormatOptions();
asserts.isequal(fmtDefault.NumericFormat, "short")
asserts.isequal(fmtDefault.LineSpacing, "compact")
asserts.isequal(fmtDefault.TruncateMatrices, "off")
fmt = nelson.display.DisplayFormatOptions('Numeric', 'LONGE', 'line', 'LOOSE', 'truncate', 'ON');
asserts.isequal(fmt.NumericFormat, "longE")
asserts.isequal(fmt.LineSpacing, "loose")
asserts.isequal(fmt.TruncateMatrices, "on")
format(fmt);
f = format();
asserts.isequal(f.NumericFormat, "longE")
asserts.isequal(f.LineSpacing, "loose")
asserts.isequal(f.TruncateMatrices, "on")
%=============================================================================
fmt.NumericFormat = "shortG";
fmt.LineSpacing = "compact";
fmt.TruncateMatrices = "off";
asserts.isequal(fmt.NumericFormat, "shortG")
asserts.isequal(fmt.LineSpacing, "compact")
asserts.isequal(fmt.TruncateMatrices, "off")
asserts.checkerror('fmt.NumericFormat = "SHORTG";', _('Invalid value for "NumericFormat".'));
asserts.checkerror('fmt.NumericFormat = "short"; fmt.NumericFormat = "sho";', _('Invalid value for "NumericFormat".'));
asserts.checkerror('fmt.NumericFormat = ["short", "long"];', _('String scalar or character vector expected.'));
asserts.checkerror('fmt.NumericFormat = 1;', _('String scalar or character vector expected.'));
asserts.checkerror('fmt.LineSpacing = "COMPACT";', _('Invalid value for "LineSpacing".'));
asserts.checkerror('fmt.LineSpacing = "comp";', _('Invalid value for "LineSpacing".'));
asserts.checkerror('fmt.TruncateMatrices = "OFF";', _('Invalid value for "TruncateMatrices".'));
asserts.checkerror('fmt.TruncateMatrices = "maybe";', _('Invalid value for "TruncateMatrices".'));
asserts.checkerror('nelson.display.DisplayFormatOptions("NumericFormat")', _('Name-value arguments expected.'));
asserts.checkerror('nelson.display.DisplayFormatOptions("Unknown", "short")', _('Invalid name-value argument name.'));
format(oldFormat);
%=============================================================================
