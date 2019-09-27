
data("faithful")
erupt <- faithful$eruptions

kurtosis = function(data){
  x = mean(data)
  n = length(data)
  
  p1 = ((n+1)*n*(n-1))/((n-2)*(n-3)) 
  
  s = sum(data-x)
  
  p2 = (s^4)/((s^2)^2)-3
  p3 = ((n-1)^2)/((n-2)*(n-3))
  t.x = (p1*p2*p3)
  return(t.x)
}

kurtosis(erupt)