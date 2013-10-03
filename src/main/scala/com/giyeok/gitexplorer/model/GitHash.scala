package com.giyeok.gitexplorer.model

import java.security.InvalidParameterException

class GitSHA1(val sha1: Seq[Byte]) extends Ordered[GitSHA1] {
    def this(sha1: String) = this(GitSHA1.arrayFromString(sha1))

    assert(sha1.length == 20)

    override def equals(other: Any): Boolean = other match {
        case that: GitSHA1 => this.sha1 == that.sha1
        case _ => false
    }

    override def hashCode = sha1.hashCode

    override def compare(other: GitSHA1): Int = {
        // NOTE Is there an API to compare two seqs?
        (sha1 zip other.sha1).foldLeft(0)({
            case (0, (a, b)) =>
                val (aa, bb) = (a.toChar & 0xff, b.toChar & 0xff)
                if (aa < bb) -1
                else if (aa > bb) 1
                else 0
            case (-1, _) => -1
            case (1, _) => 1
        })
    }

    override def toString = string

    lazy val string = {
        (sha1 map { x => ((x.toInt & 0xff) | 0x100).toHexString.substring(1) }).mkString
    }
}
object GitSHA1 {
    def apply(sha1: Seq[Byte]) = new GitSHA1(sha1)
    def apply(sha1: String) = new GitSHA1(sha1)
    def unapply(sha1: String): Option[GitSHA1] = arrayFromStringOpt(sha1) match {
        case Some(array) => Some(GitSHA1(array))
        case _ => None
    }
    def arrayFromStringOpt(s: String): Option[Seq[Byte]] = {
        class InvalidSHA1Exception extends Exception
        try {
            val ss = ((1 to (40 - s.length)) map { _ => "0" } mkString) + s
            def hexCharToByte(x: Char) = x match {
                case n if ('0' to '9') contains n => n - '0'
                case a if ('a' to 'f') contains a => a - 'a' + 10
                case a if ('A' to 'f') contains a => a - 'A' + 10
                case _ => throw new InvalidSHA1Exception
            }
            Some(for (i <- (0 until 40 by 2)) yield {
                val x1 = hexCharToByte(ss.charAt(i))
                val x2 = hexCharToByte(ss.charAt(i + 1))
                (x1 * 16 + x2).toByte
            })
        } catch {
            case _: InvalidSHA1Exception => None
        }
    }
    def arrayFromString(s: String): Seq[Byte] = {
        arrayFromStringOpt(s) match {
            case Some(array) => array
            case _ => throw new InvalidParameterException
        }
    }
}
