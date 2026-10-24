%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% datenum('12:34:') used to crash the process (std::stoi on an unmatched
% optional regex group). Expected behavior parses it exactly like '12:34':
% January 1st of the current year plus the time of day.
r = datevec(now());
asserts.isequal(datenum('12:34:'), datenum([r(1) 1 1 12 34 0]));
asserts.isequal(datenum('12:34:'), datenum('12:34'));
%=============================================================================
% seconds still honored when present
asserts.isequal(datenum('12:34:56'), datenum([r(1) 1 1 12 34 56]));
%=============================================================================
