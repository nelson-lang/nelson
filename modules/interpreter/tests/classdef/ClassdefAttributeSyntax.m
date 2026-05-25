classdef ClassdefAttributeSyntax < handle
  properties (Dependent)
    DependentValue
  end

  properties (GetObservable, SetObservable)
    ObservableValue = 0
  end

  events
    ObservableValueChanged
  end
end
