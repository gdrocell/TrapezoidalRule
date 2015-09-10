/**
 * @author Gary Drocella
 * @date 09/10/2015
 * Description: Perform Trapezoidal Rule for integral approximation.
 */
interface IntegralApproximation {
   double approximate(Closure<Double> f, double x0, double xf)
}

class TrapezoidalRule implements IntegralApproximation {
   def k;

   def TrapezoidalRule(k) {
      this.k = k
   }

   double approximate(Closure<Double> f, double x0, double xf) {
      def h = (xf - x0) / k
      def xl = x0
      def area = 0;
      while(Math.abs(xl - xf) > 0.00000001) {
         def f0 = f(xl)
         def xu = xl + h
         def f1 = f(xu)
         area += (xu-xl)*((f0 + f1)/2);
         xl += h
      }
      return area;
   }

   def setK(k) {
      this.k = k;
   }

   def getK() {
      return this.k;
   } 
}

def tr = new TrapezoidalRule(5);
println "approximate integral of f(x)=sqrt((sin(x)^3)+1) over the interval [0,1]  =" + tr.approximate({x -> Math.sqrt(Math.pow(Math.sin(x), 3) + 1) }, 0, 1);
println "approximate integral of f(x)=e^(x^2) over the interval [0,1] =" + tr.approximate({x -> Math.pow(Math.E, Math.pow(x , 2 ) ) }, 0, 1);