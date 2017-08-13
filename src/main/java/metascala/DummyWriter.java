package metascala;


class DummyWriter extends java.io.Writer {
    public void flush(){/*do nothing*/}
    public void close(){/*do nothing*/}
    public void write(char[] cbuf, int off, int len){/*do nothing*/}
}
