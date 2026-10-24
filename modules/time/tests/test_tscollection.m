%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
ts = timeseries([1; 2; 3], [10; 11; 12], 'Name', 'speed');
tsc = tscollection(ts, 'Name', 'run');
asserts.isequal(tsc.Name, 'run');
asserts.isequal(tsc.Length, 3);
asserts.isequal(length(tsc), 3);
asserts.isequal(isempty(tsc), false);
asserts.isequal(size(tsc), [3 1]);
asserts.isequal(gettimeseriesnames(tsc), {'speed'});
asserts.isequal(fieldnames(tsc), {'speed'});
asserts.isequal(tsc.speed.Data, [1; 2; 3]);
asserts.isequal(get(tsc, 'speed').Data, [1; 2; 3]);
asserts.isequal(tsc(end).speed.Data, 3);
printed = evalc('display(tsc)');
asserts.istrue(contains(printed, 'tsc ='));
asserts.istrue(contains(printed, 'Time Series Collection Object: run'));
savedState = struct(tsc);
loaded = tscollection.loadobj(savedState);
asserts.isequal(loaded.Name, tsc.Name);
asserts.isequal(loaded.Time, tsc.Time);
asserts.isequal(loaded.speed.Data, tsc.speed.Data);
tsc = set(tsc, 'Name', 'run2');
asserts.isequal(tsc.Name, 'run2');
%=============================================================================
ts1 = timeseries([1.1 2.9 3.7 4.0 3.0]', 1:5, 'Name', 'Acceleration');
ts2 = timeseries([3.2 4.2 6.2 8.5 1.1]', 1:5, 'Name', 'Speed');
ts = tscollection({ts1; ts2});
asserts.isequal(gettimeseriesnames(ts), {'Acceleration', 'Speed'});
R = evalc('ts');
asserts.istrue(contains(R, 'Time Series Collection Object: unnamed'));
asserts.istrue(contains(R, 'Start time'));
asserts.istrue(contains(R, '1 seconds'));
asserts.istrue(contains(R, 'End time'));
asserts.istrue(contains(R, '5 seconds'));
asserts.istrue(contains(R, 'Acceleration'));
asserts.istrue(contains(R, 'Speed'));
%=============================================================================
tsc = addts(tsc, timeseries([10; 20; 30], [10; 11; 12], 'Name', 'power'));
asserts.isequal(length(gettimeseriesnames(tsc)), 2);
part = getsampleusingtime(tsc, 11, 12);
asserts.isequal(part.speed.Data, [2; 3]);
%=============================================================================
tsc = resample(tsc, [10; 10.5; 11; 12], 'linear');
asserts.isequal(tsc.Length, 4);
tsc = setabstime(tsc, '01-Jan-2024');
asserts.isequal(tsc.TimeInfo.StartDate, '01-Jan-2024');
asserts.isequal(iscell(getabstime(tsc)), true);
tsc = removets(tsc, 'power');
asserts.isequal(gettimeseriesnames(tsc), {'speed'});
tsc = addsampletocollection(tsc, 'Time', 13, 'Data', 8);
asserts.isequal(tsc.Length, 5);
tsc = delsamplefromcollection(tsc, 'Index', 5);
asserts.isequal(tsc.Length, 4);
%=============================================================================
speed = timeseries([1; 2; 3], [1; 2; 3], 'Name', 'speed');
flow = timeseries([10; 20; 30], [1; 2; 3], 'Name', 'flow');
traffic = tscollection(speed, 'Name', 'traffic');
traffic = addts(traffic, flow);
traffic = addsampletocollection(traffic, 'Time', 3.25, 'speed', 5);
asserts.isequal(traffic.speed.Data(end), 5);
asserts.istrue(isnan(traffic.flow.Data(end)));
asserts.isequal(traffic.Time(end), 3.25);
traffic = delsamplefromcollection(traffic, 'Index', find(isnan(traffic.flow.Data)));
asserts.isequal(traffic.Time, [1; 2; 3]);
%=============================================================================
tsc.speed = timeseries([4; 5; 6; 7], tsc.Time, 'Name', 'speed');
asserts.isequal(tsc.speed.Data, [4; 5; 6; 7]);
tsc = setTimeseriesName(tsc, 'speed', 'velocity');
asserts.isequal(gettimeseriesnames(tsc), {'velocity'});
tsc = settimeseriesnames(tsc, {'speed'});
asserts.isequal(gettimeseriesnames(tsc), {'speed'});
props = properties(tsc);
asserts.isequal(any(strcmp(props, 'Time')), true);
asserts.isequal(any(strcmp(props, 'speed')), true);
%=============================================================================
left = tscollection(timeseries([1; 2], [1; 2], 'Name', 'a'));
right = tscollection(timeseries([3; 4], [1; 2], 'Name', 'b'));
joined = [left right];
asserts.isequal(gettimeseriesnames(joined), {'a', 'b'});
stacked = [left; left];
asserts.isequal(stacked.Length, 4);
%=============================================================================
