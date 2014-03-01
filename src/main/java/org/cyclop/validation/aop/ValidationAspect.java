package org.cyclop.validation.aop;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.validation.BeanValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Named;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

@Aspect
@Named
public class ValidationAspect {

	private final static Logger LOG = LoggerFactory.getLogger(ValidationAspect.class);

	@Pointcut("execution(* org.cyclop..*.*(..)) && @within(org.cyclop.validation.EnableValidation)")
	protected void validate() {
	}

	@Around("validate()")
	protected Object doValidate(ProceedingJoinPoint pjp) throws Throwable {
		try {
			executeParamValidation(pjp);
		} catch (BeanValidationException be) {
			throw be;
		} catch (Exception e) {
			LOG.error("Error executing param validation", e);
		}

		Object response = pjp.proceed();

		try {
			executeResponseValidation(pjp, response);
		} catch (BeanValidationException be) {
			throw be;
		} catch (Exception e) {
			LOG.error("Error executing response validation", e);
		}

		return response;
	}

	private void executeResponseValidation(ProceedingJoinPoint pjp, Object response) {
		MethodSignature sig = (MethodSignature) pjp.getSignature();
		Method method = sig.getMethod();

		BeanValidator validator = null;
		for (Annotation respAnnotatioin : method.getDeclaredAnnotations()) {
			if (respAnnotatioin.annotationType().isAssignableFrom(NotNull.class) ||
					(respAnnotatioin.annotationType().isAssignableFrom(Valid.class) && response != null)) {
				if (validator == null) {
					validator = createValidator(pjp);
				}
				validator.add("METHOD_RETURN_VALUE", response);
			}
		}

		if (validator != null) {
			validator.validate();
		}
	}

	private void executeParamValidation(ProceedingJoinPoint pjp) {
		Object[] args = pjp.getArgs();
		MethodSignature sig = (MethodSignature) pjp.getSignature();
		Method method = sig.getMethod();
		Annotation[][] allParamAnnotations = method.getParameterAnnotations();

		BeanValidator validator = null;
		for (int paramIdx = 0; paramIdx < allParamAnnotations.length; paramIdx++) {
			Annotation[] paramAnnotations = allParamAnnotations[paramIdx];
			if (paramAnnotations == null || paramAnnotations.length == 0) {
				continue;
			}
			Object obj = args[paramIdx];
			if (contains(paramAnnotations, NotNull.class) || (contains(paramAnnotations, Valid.class) && obj != null)) {
				if (validator == null) {
					validator = createValidator(pjp);
				}
				validator.add("METHOD_PARAM_INDEX_" + paramIdx, obj);
			}
		}
		if (validator != null) {
			validator.validate();
		}
	}

	private BeanValidator createValidator(ProceedingJoinPoint pjp) {
		BeanValidator validator = BeanValidator.create(pjp.getSignature().toString());
		return validator;
	}

	private boolean contains(Annotation[] annotations, Class<?> annotation) {
		for (Annotation ann : annotations) {
			if (ann.annotationType().isAssignableFrom(annotation)) {
				return true;
			}
		}
		return false;
	}
}
