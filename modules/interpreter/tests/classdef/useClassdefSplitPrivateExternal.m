function r = useClassdefSplitPrivateExternal()
  try
    privateExternal(ClassdefSplit());
    r = 'allowed';
  catch
    r = 'blocked';
  end
end
