package gregd.cspsolve.log

import org.apache.logging.log4j.Logger
import org.apache.logging.log4j.LogManager


class MyLogger(name : String) {
    val logger = LogManager.getLogger(name)
	def debug(msg: => AnyRef) = if (logger.isDebugEnabled) logger.debug(msg)
	def info(msg: => AnyRef) = if (logger.isInfoEnabled) logger.info(msg)
	def warn(msg: => AnyRef) = if (logger.isWarnEnabled) logger.warn(msg)
	def error(msg: => AnyRef) = if (logger.isErrorEnabled) logger.error(msg)
	def trace(msg: => AnyRef) = if (logger.isTraceEnabled) logger.trace(msg)
}