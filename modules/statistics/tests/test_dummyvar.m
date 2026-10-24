%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('dummyvar'), 1);
asserts.isequal(nargout('dummyvar'), 1);
%=============================================================================
Colors = categorical({'Red'; 'Blue'; 'Green'; 'Red'; 'Green'; 'Blue'});
D = dummyvar(Colors);
asserts.isequal(D, [0 0 1; 1 0 0; 0 1 0; 0 0 1; 0 1 0; 1 0 0]);
%=============================================================================
machine = [1 1 1 1 2 2 2 2]';
operator = [1 2 3 1 2 3 1 2]';
D = dummyvar([machine operator]);
asserts.isequal(D, [1 0 1 0 0; ...
                   1 0 0 1 0; ...
                   1 0 0 0 1; ...
                   1 0 1 0 0; ...
                   0 1 0 1 0; ...
                   0 1 0 0 1; ...
                   0 1 1 0 0; ...
                   0 1 0 1 0]);
%=============================================================================
phone = {'mobile'; 'landline'; 'mobile'; 'mobile'; 'mobile'; 'landline'; 'landline'};
codes = [802 802 603 603 802 603 802]';
newcodes = categorical(codes);
D = dummyvar({phone, newcodes});
asserts.isequal(D, [1 0 0 1; ...
                   0 1 0 1; ...
                   1 0 1 0; ...
                   1 0 1 0; ...
                   1 0 0 1; ...
                   0 1 1 0; ...
                   0 1 0 1]);
%=============================================================================
D = dummyvar([1 NaN 3]');
asserts.isequal(D(1, :), [1 0 0]);
asserts.istrue(all(isnan(D(2, :))));
asserts.isequal(D(3, :), [0 0 1]);
%=============================================================================
D = dummyvar(single([1 2 1]'));
asserts.isequal(class(D), 'single');
asserts.isequal(D, single([1 0; 0 1; 1 0]));
%=============================================================================
D = dummyvar({'b'; ''; 'a'; 'b'});
asserts.isequal(D(1, :), [1 0]);
asserts.istrue(all(isnan(D(2, :))));
asserts.isequal(D(3:4, :), [0 1; 1 0]);
%=============================================================================
asserts.checkerror('dummyvar()', _('Not enough input arguments.'));
asserts.checkerror('dummyvar({[1;2], [1;2;3]})', sprintf(_('%s must have the same number of rows.'), 'Grouping variables'));
asserts.checkerror('dummyvar([0 1])', _('Numeric GROUP values must be positive integers or NaN.'));
asserts.checkerror('dummyvar({struct()})', sprintf(_('%s must be a grouping variable.'), 'GROUP'));
%=============================================================================
