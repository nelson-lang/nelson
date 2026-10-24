%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
ts = timeseries((1:4)', (10:13)', 'Name', 'speed');
asserts.isequal(istimeseries(ts), true);
asserts.isequal(ts.Name, 'speed');
asserts.isequal(ts.Length, 4);
asserts.isequal(ts.Time, (10:13)');
asserts.isequal(ts.Data, (1:4)');
asserts.isequal(length(ts), 4);
asserts.isequal(isempty(ts), false);
asserts.isequal(get(ts, 'Name'), 'speed');
printed = evalc('display(ts)');
asserts.istrue(contains(printed, 'ts ='));
asserts.istrue(contains(printed, 'timeseries: 4 samples'));
savedState = struct(ts);
loaded = timeseries.loadobj(savedState);
asserts.isequal(loaded.Name, ts.Name);
asserts.isequal(loaded.Time, ts.Time);
asserts.isequal(loaded.Data, ts.Data);
ts3 = timeseries(ones(2, 3, 4));
asserts.isequal(ts3.IsTimeFirst, false);
asserts.isequal(ts3.Length, 4);
asserts.isequal(numel(ts3), 1);
asserts.isequal(size(ts3), [2 3 4]);
[d1, d2, d3] = size(ts3);
asserts.isequal([d1 d2 d3], [2 3 4]);
asserts.isequal(size(ts3, 3), 4);
asserts.isequal(isempty(ts3), false);
ts = set(ts, 'UserData', 12);
asserts.isequal(ts.UserData, 12);
ts.Quality = [0; 1; 0; 1];
ts.QualityInfo = tsdata.qualmetadata('Code', [0 1], 'Description', {'ok', 'bad'});
asserts.isequal(ts.Quality, [0; 1; 0; 1]);
asserts.isequal(getqualitydesc(ts, [0 1]), {'ok'; 'bad'});
%=============================================================================
sample = getsamples(ts, 2:3);
asserts.isequal(sample.Time, [11; 12]);
asserts.isequal(getdatasamples(ts, 2), 2);
asserts.isequal(getdatasamplesize(ts), [1 1]);
asserts.isequal(getsampleusingtime(ts, 11, 12).Data, [2; 3]);
asserts.isequal(ts(1:2).Data, [1; 2]);
position = timeseries([1 10; 2 20; 3 30], [1; 2; 3]);
asserts.isequal(getdatasamplesize(position), [1 2]);
%=============================================================================
ts = addsample(ts, 'Time', 14, 'Data', 5);
asserts.isequal(ts.Time, (10:14)');
ts = delsample(ts, 'Index', 5);
asserts.isequal(ts.Time, (10:13)');
appended = append(ts, timeseries(5, 14));
asserts.isequal(appended.Length, 5);
%=============================================================================
ev = tsdata.event('middle', 12);
ts = addevent(ts, ev);
asserts.isequal(gettsatevent(ts, 'middle').Time, 12);
asserts.isequal(gettsbeforeevent(ts, 'middle').Time, [10; 11]);
asserts.isequal(gettsafterevent(ts, 'middle').Time, 13);
asserts.isequal(gettsbeforeatevent(ts, 'middle').Time, [10; 11; 12]);
asserts.isequal(gettsafteratevent(ts, 'middle').Time, [12; 13]);
asserts.isequal(gettsbetweenevents(addevent(ts, tsdata.event('end', 13)), 'middle', 'end').Time, [12; 13]);
ts = delevent(ts, 'middle');
asserts.isequal(numel(ts.Events), 0);
%=============================================================================
ts = setinterpmethod(ts, 'nearest');
asserts.isequal(getinterpmethod(ts), 'nearest');
rs = resample(ts, [10; 10.5; 11]);
asserts.isequal(rs.Time, [10; 10.5; 11]);
[a, b] = synchronize(timeseries([1; 2], [1; 2]), timeseries([10; 30], [1; 3]));
asserts.isequal(a.Time, [1; 2; 3]);
asserts.isequal(b.Time, [1; 2; 3]);
ts = setuniformtime(ts, 'StartTime', 0, 'Interval', 2, 'Length', 4);
asserts.isequal(ts.Time, [0; 2; 4; 6]);
ts = setabstime(ts, '01-Jan-2024');
asserts.isequal(iscell(getabstime(ts)), true);
d = detrend(timeseries([1; 2; 3], [1; 2; 3]), 'constant');
asserts.isequal(d.Data, [-1; 0; 1]);
f = filter(timeseries([1; 2; 3], [1; 2; 3]), 1, 1);
asserts.isequal(f.Data, [1; 2; 3]);
f = filter(timeseries([1; 2; 3], [1; 2; 3]), [1 1] / 2, 1);
asserts.isequal(f.Data, [0.5; 1.5; 2.5]);
idf = idealfilter(timeseries([1; 2; 3], [1; 2; 3]), [0 1]);
asserts.isequal(size(idf.Data), [3 1]);
%=============================================================================
asserts.isequal(mean(timeseries([1; 2; 3])), 2);
asserts.isequal(sum(timeseries([1; 2; 3])), 6);
asserts.isequal(min(timeseries([1; 2; 3])), 1);
asserts.isequal(max(timeseries([1; 2; 3])), 3);
asserts.isequal(median(timeseries([1; 2; 3])), 2);
asserts.isequal(mode(timeseries([1; 2; 2; 3])), 2);
asserts.isequal(var(timeseries([1; 2; 3])), 1);
asserts.isequal(std(timeseries([1; 2; 3])), 1);
asserts.isequal(iqr(timeseries([1; 2; 3; 4])), 1.5);
%=============================================================================
left = timeseries([1; 2], [1; 2]);
right = timeseries([10; 20], [1; 2]);
asserts.isequal((left + right).Data, [11; 22]);
asserts.isequal((right - left).Data, [9; 18]);
asserts.isequal((left .* 2).Data, [2; 4]);
asserts.isequal((right ./ 10).Data, [1; 2]);
asserts.isequal((10 .\ right).Data, [1; 2]);
asserts.isequal((left * 2).Data, [2; 4]);
asserts.isequal((right / 10).Data, [1; 2]);
asserts.isequal((10 \ right).Data, [1; 2]);
asserts.isequal((-left).Data, [-1; -2]);
asserts.isequal((+left).Data, [1; 2]);
asserts.isequal((left == left).Data, [true; true]);
asserts.isequal(isequalwithequalnans(left, left), true);
%=============================================================================
asserts.checkerror('timeseries([1; 2], [1; 2; 3])', sprintf(_('%s and time lengths must match.'), 'Data'));
%=============================================================================
