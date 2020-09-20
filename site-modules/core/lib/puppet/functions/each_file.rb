Puppet::Functions.create_function(:'each_file') do
  dispatch :each_file do
    param 'String', :path
    block_param 'Callable[1, 1]', :block
  end

  def each_file(path)
    Dir.glob(path) do |filename|
      yield(*filename)
    end
  end
end
