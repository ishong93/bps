// Integration operators for JSAINT.
// Translated from jsops.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * JSOps defines integration operators used by JSAINT.
 * Each operator represents an integration method.
 */
public class JSOps {

    /** An integration operator definition. */
    public static class IntegrationOp {
        public String name;
        public java.util.function.Predicate<Object> test;

        public IntegrationOp(String name, java.util.function.Predicate<Object> test) {
            this.name = name;
            this.test = test;
        }
    }

    /** Get standard integration operators. Corresponds to jsops.lisp. */
    @SuppressWarnings("unchecked")
    public static List<IntegrationOp> standardOps() {
        List<IntegrationOp> ops = new ArrayList<>();

        // Integral-of-Constant: (Integral ?t ?var) where ?var not in ?t
        ops.add(new IntegrationOp("Integral-of-Constant", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            return !Simplify.occursIn(parts.get(2), parts.get(1));
        }));

        // Integral-of-Self: (Integral ?exp ?exp)
        ops.add(new IntegrationOp("Integral-of-Self", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            return JData.deepEqual(parts.get(1), parts.get(2));
        }));

        // Integral-of-Sum: (Integral (+ ?t1 ?t2) ?var)
        ops.add(new IntegrationOp("Integral-of-Sum", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            return inner != null && inner.size() >= 3 && "+".equals(inner.get(0));
        }));

        // Move-Constant-Outside: (Integral (* ?const ?nonconst) ?var)
        ops.add(new IntegrationOp("Move-Constant-Outside", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            if (inner == null || inner.size() != 3 || !"*".equals(inner.get(0))) return false;
            return !Simplify.occursIn(parts.get(2), inner.get(1))
                && Simplify.occursIn(parts.get(2), inner.get(2));
        }));

        // Simple-e-integral: (Integral (expt %e ?var) ?var)
        ops.add(new IntegrationOp("Simple-e-integral", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            return inner != null && inner.size() == 3 && "expt".equals(inner.get(0))
                && "%e".equals(inner.get(1)) && JData.deepEqual(inner.get(2), parts.get(2));
        }));

        // Sin-integral: (Integral (sin (* ?a ?var)) ?var)
        ops.add(new IntegrationOp("Sin-integral", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            if (inner == null || inner.size() != 2 || !"sin".equals(inner.get(0))) return false;
            List<Object> product = asList(inner.get(1));
            return product != null && product.size() == 3 && "*".equals(product.get(0))
                && !Simplify.occursIn(parts.get(2), product.get(1));
        }));

        // Cos-integral: (Integral (cos (* ?a ?var)) ?var)
        ops.add(new IntegrationOp("Cos-integral", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            if (inner == null || inner.size() != 2 || !"cos".equals(inner.get(0))) return false;
            List<Object> product = asList(inner.get(1));
            return product != null && product.size() == 3 && "*".equals(product.get(0))
                && !Simplify.occursIn(parts.get(2), product.get(1));
        }));

        // Log-integral: (Integral (log ?var %e) ?var)
        ops.add(new IntegrationOp("Log-integral", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            return inner != null && inner.size() == 3 && "log".equals(inner.get(0))
                && JData.deepEqual(inner.get(1), parts.get(2)) && "%e".equals(inner.get(2));
        }));

        // Integral-of-Polyterm: (Integral (expt ?var ?n) ?var) where ?n != -1
        ops.add(new IntegrationOp("Integral-of-Polyterm", integral -> {
            List<Object> parts = asList(integral);
            if (parts == null || parts.size() != 3) return false;
            if (!"Integral".equals(parts.get(0))) return false;
            List<Object> inner = asList(parts.get(1));
            if (inner == null || inner.size() != 3 || !"expt".equals(inner.get(0))) return false;
            return JData.deepEqual(inner.get(1), parts.get(2))
                && !Simplify.sameConstant(inner.get(2), -1);
        }));

        return ops;
    }

    @SuppressWarnings("unchecked")
    private static List<Object> asList(Object x) {
        if (x instanceof List) return (List<Object>) x;
        return null;
    }
}
