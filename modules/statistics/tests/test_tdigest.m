%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--REUSE PROCESS-->
td = tdigest();
asserts.isequal(class(td), 'tdigest');
asserts.istrue(isa(td, 'tdigest'));
asserts.istrue(isobject(td));
asserts.isequal(size(td), [1 1]);
asserts.isequal(numel(td), 1);
asserts.isequal(td.compression, 100);
asserts.isequal(td.totalWeight, 0);
asserts.isequal(td.quantile([0 0.5 1]), [NaN; NaN; NaN]);
props = properties(td);
asserts.isequal(sort(props(:)), {'compression'; 'totalWeight'});
methodList = methods(td);
asserts.istrue(any(strcmp(methodList, 'compress')));
asserts.istrue(any(strcmp(methodList, 'percentile')));
asserts.istrue(any(strcmp(methodList, 'quantile')));
asserts.isfalse(any(strcmp(methodList, 'means')));
asserts.isfalse(any(strcmp(methodList, 'weights')));
asserts.isfalse(any(strcmp(methodList, 'shouldCompress')));
%=============================================================================
td = tdigest(50);
asserts.isequal(td.compression, 50);
asserts.isequal(td.totalWeight, 0);
%=============================================================================
td = tdigest(100, [1 2 3 NaN]);
asserts.isequal(td.compression, 100);
asserts.isequal(td.totalWeight, 3);
asserts.isequal(td.percentile([0 50 100]), [1; 2; 3]);
%=============================================================================
td = tdigest();
td = td + linspace(-3, 3, 1000);
R1 = td.quantile([0.05, 0.50, 0.95]);
R2 = td.percentile([5, 50, 95]);
asserts.isequal(R1, R2);
asserts.isapprox(R1, [-2.7027; 0; 2.7027], 1e-4);
%=============================================================================
td = tdigest([100 NaN 300]);
R = td.percentile([5, 50, 95]);
REF = [100; 200; 300];
asserts.isequal(R, REF);
%=============================================================================
td = tdigest(100);
a = 1:5000;
td = td + a;
R = td.percentile([5, 50, 95]);
REF = [2.505000000000000e+02;
     2.500500000000000e+03;
     4.750500000000000e+03];
asserts.isapprox(R, REF, 1e1);
%=============================================================================
td = tdigest(100);
a = 5000:-1:1;
td = td + a;
R = td.percentile([5, 50, 95]);
REF = [2.505000000000000e+02;
     2.500500000000000e+03;
     4.750500000000000e+03];
asserts.isapprox(R, REF, 1e1);
%=============================================================================
td = tdigest(100);
a = cos(5000:-1:1);
td = td + a;
R = td.percentile([5, 50, 95]);
REF = [-0.9874;
   -0.0000;
    0.9874];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
td = tdigest(100);
td = (1:10) + td;
asserts.isequal(td.totalWeight, 10);
asserts.isapprox(td.percentile([0 50 100]), [1; 5.5; 10], 1e-12);
%=============================================================================
td = tdigest(100);
td = td + [1 NaN 3];
asserts.isequal(td.totalWeight, 2);
asserts.isequal(td.percentile([0 50 100]), [1; 2; 3]);
%=============================================================================
td = tdigest(100, 1:500);
before = td.percentile([5 50 95]);
td = td.compress();
after = td.percentile([5 50 95]);
asserts.isapprox(after, before, 10);
%=============================================================================
try
  tdigest(0);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _('Compression parameter must be a positive scalar number.'));
end
try
  tdigest(-1);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _('Compression parameter must be a positive scalar number.'));
end
try
  tdigest(Inf);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _('Compression parameter must be a positive scalar number.'));
end
try
  tdigest(100, 'abc');
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, sprintf(_('%s must be numeric.'), 'Data'));
end
try
  tdigest(100, 1 + i);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, sprintf(_('%s must be numeric.'), 'Data'));
end
try
  td = tdigest();
  td.totalWeight = -1;
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _('Total weight must be a nonnegative scalar number.'));
end
%=============================================================================
td = tdigest();
try
  td.quantile([-0.1 0.5]);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _('Value must be at least 0.'));
end
try
  td.quantile([0.5 1.1]);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, sprintf(_('Value must be less than or equal to %d.'), 1));
end
try
  td.percentile([-1 50]);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _('Value must be at least 0.'));
end
try
  td.percentile([50 101]);
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, sprintf(_('Value must be less than or equal to %d.'), 100));
end
try
  td.quantile(ones(2, 2));
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, sprintf(_('%s must be a vector.'), 'Input quantiles'));
end
try
  [td td];
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _("Combining 'tdigest' objects is not allowed as they are designed to be scalar."));
end
try
  [td; td];
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _("Combining 'tdigest' objects is not allowed as they are designed to be scalar."));
end
try
  td + td;
  error(_('Expected an error.'));
catch M
  asserts.isequal(M.message, _("Combining 'tdigest' objects is not allowed as they are designed to be scalar."));
end
%=============================================================================
