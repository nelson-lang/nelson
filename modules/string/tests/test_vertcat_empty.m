%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = vertcat(char(zeros(0, 0)), 'A');
asserts.isequal(R, 'A');
R = vertcat(char(zeros(1, 0)), 'A');
asserts.isequal(R, 'A');
R = vertcat(char(zeros(0, 1)), 'A');
asserts.isequal(R, 'A');
%=============================================================================
R = [char(65); 1];
asserts.isequal(class(R), 'char');
asserts.isequal(double(R), [65; 1]);
%=============================================================================
R = [1; char(65)];
asserts.isequal(class(R), 'char');
asserts.isequal(double(R), [1; 65]);
%=============================================================================
R = [char(65); int8(-1)];
asserts.isequal(class(R), 'char');
asserts.isequal(double(R), [65; 0]);
%=============================================================================
R = [char(65); 65536];
asserts.isequal(class(R), 'char');
asserts.isequal(double(R), [65; 65535]);
%=============================================================================
msg = sprintf(_('The following error occurred converting from %s to %s:\n%s'), 'logical', 'char', _('Conversion to char from logical is not possible.'));
asserts.checkerror('R = [char(65); true];', msg, 'Nelson:UnableToConvert');
%=============================================================================
